
(ns app.world
    (:require ["rot-js" :as rot]
              [clojure.set :as set]))

(declare chase! compute-fov update-vis!)

;; just functions for querying and modifying world state
;;   probably shouldn't spawn any processes in here


;; mutable world state
;;   may be read/written concurrently by outside processes
;;   for entities, duplicate the hashkey inside the entity record
;;    -- this allows for cleaner code in some places & avoids searching
;;    :action is any function that gets called on the entity's turn
;;    -- it is not really an "act" method
(defonce world-state (atom {:grid {}
                            :seen #{}
                            :visible {}
                            :entities {:player {:id :player :type :local-player 
                                                :fov-fn :fov-360 :vision 10 :diag-time 14}
                                       :pedro {:id :pedro :type :npc 
                                               :fov-fn :fov-90 :vision 5
                                               :action #(chase! :pedro :player)}}
                            }))

;; modifying multiple keys in a hashmap
;;   with a static value v:
(defn assoc-multi [coll ks v]
  (reduce #(assoc %1 %2 v) coll ks))

;;   with a function of the current value:
(defn update-multi [coll ks f]
  (reduce #(update %1 %2 f) coll ks))

;; updates all keys in coll
(defn update-all [coll f]
  (update-multi coll (keys coll) f))

;; n random items from a collection
(defn sample [n coll]
  (take n (shuffle coll)))

;; grid directions are stored in rot-js as a javascript object
;;   convert to clj vector
(def dirs (get (js->clj (. rot -DIRS)) "8"))

;; rot-js map generator uses callback fn to modify an object
;;   this generates and returns that fn
(defn- digger-callback [grid-ref]
  (fn [x y v]
    (if (= v 1)
      nil ;;1 = wall, don't store walls
      (swap! grid-ref assoc [x y] :empty-grid)))) ;;store only empty spaces

;; create a temporary mutable ref for the rot-js map generator to use
;;   then return an ordinary clj hashmap
(defn- generate-grid [rot-digger]
  (let [grid-ref (atom {})]
    (.create rot-digger (digger-callback grid-ref))
    (into {} @grid-ref)))

(defn- insert-boxes [grid ks]
  (assoc-multi grid ks :closed-box))

;; create a starting map
(defn init-grid! [[w h]]
  (let [grid (generate-grid (rot/Map.Digger. w h)) ;;initial map
        grid-keys (keys grid)
        box-keys (sample 5 grid-keys)  ;;random locations for boxes
        free-cells (set/difference (set grid-keys) (set box-keys)) ;;squares without boxes
        starting-entities (:entities @world-state)
        starting-coords (sample (count starting-entities) free-cells) ;;a vector of random starting locations
        ]
    ;; threading macro (->) creates a conjugate function
    ;; -- avoids having multiple swap!s in a row
    (swap! world-state #(-> %
                        ;;add boxes to the map and update the world state
                            (assoc :grid (insert-boxes grid box-keys))
                        ;;give each starting entity a location (:coords key) and random facing (:delta key)
                        ;;  an entity is a k-v pair where each v is a hashmap
                            (assoc :entities
                                   (into {} (map (fn [[k v] x] [k (assoc v :coords x :delta (rand-nth dirs))])
                                                 starting-entities starting-coords)))
                        ;;update world state with location of ananas
                            (assoc :ananas (first box-keys))))
    ;; compute entities' fov based on starting info
    ;; threading macro (->) uses result of swap! as first arg to next function
    (->
     (swap! world-state update :entities update-all compute-fov)
     (get-in [:entities :player :fov])
     (update-vis!))
    ))

;; rot-js pathfinder needs a callback fn to determine if a cell is passable
;;  this generates and returns that fn
(defn- pass-callback []
  (fn [x y]
    (contains? (:grid @world-state) [x y])))

;; rot-js FOV needs a callback to determine if a cell is transparent
(defn- light-callback []
  (fn [x y]
    (contains? (:grid @world-state) [x y])))

;; rot-js pathfinder builds a path using a callback fn to mutate an object
;;  this generates and returns that fn
(defn- path-callback [path-ref]
  (fn [x y]
    (swap! path-ref conj [x y])))

;; rot-js FOV returns data using a callback
;;   x and y coords, r = distance, v = visibility
(defn- fov-callback [data-ref]
  (fn [x y r v]
    (swap! data-ref assoc [x y] v)
    ))

;;precise shadowcasting; 360 degrees only, but provides degrees of visibility
(defonce FOV-P (rot/FOV.PreciseShadowcasting. (light-callback)))

;;recursive shadowcasting; supports 90 and 180 degrees; binary visibility only
(defonce FOV-R (rot/FOV.RecursiveShadowcasting. (light-callback)))

;; fov computation: takes an entity record
;; multimehtod dispatch based on :fov-fn
(defmulti compute-fov #(:fov-fn %))

;;fov functions used by entities
;;  create a tmp mutable ref for callback fn, then returns entity with fov data
(defmethod compute-fov :fov-360 [{:keys [coords vision] :as entity}]
  (let [data-ref (atom {})
        [x y] coords]
    (.compute FOV-P x y vision (fov-callback data-ref))
    (assoc entity :fov @data-ref)
    ))

(defmethod compute-fov :fov-90 [{:keys [coords vision delta] :as entity}]
  (let [data-ref (atom {})
        [x y] coords
        dir (.indexOf dirs delta)]
    (.compute90 FOV-R x y vision dir (fov-callback data-ref))
    (assoc entity :fov @data-ref)
    ))

;;updates world visibility info based on given fov data
(defn update-vis! [fovmap]
  (swap! world-state #(-> %
                          (assoc :visible fovmap)
                          (update :seen into (keys fovmap))
                          )))

;; create a temporary mutable ref for rot-js pathfinder to use
;;   then return an ordinary clj vector
;; start and destination are coord pairs
;;   topo = # of movement directions
(defn- compute-path [[start-x start-y] [dest-x dest-y] topo]
  (let [astar (rot/Path.AStar. dest-x dest-y (pass-callback) #js{:topology topo})
        path-ref (atom [])]
    (.compute astar start-x start-y (path-callback path-ref))
    (into [] @path-ref)))

;; mutate the :coords of the provided entity within world-state
;; path is a vector of coordinate pairs
;;   rot-js pathfinder includes the starting point as the first item in the path
;; returns remanining path length
(defn- follow-path! [entity-key [start-coords next-step & rest-of-path]]
  (when next-step
    ;;save the change from previous coordinates
    (let [delta (mapv - next-step (get-in @world-state [:entities entity-key :coords]))]
      ;;update coordinates and coordinate change
      (swap! world-state update-in [:entities entity-key] 
             assoc :coords next-step :delta delta)
      ;;update fov using new info
      ;;TODO put this in a general "move" fn
      (swap! world-state update-in [:entities entity-key] compute-fov)
      ))
  {:path-length (count rest-of-path)})

;; entity behavior: move towards target entity
;;   modifies entity coords in world state
;;   returns result of follow-path!
(defn chase! [entity-key target-key]
  (let [s @world-state
        e-coords (get-in s [:entities entity-key :coords])
        t-coords (get-in s [:entities target-key :coords])
        topology 4]
    (follow-path! entity-key (compute-path e-coords t-coords topology))
    ))

;; moves player in a given grid direction
;;   updates world state
;;   returns result if player was moved, otherwise nil
(defn move-player! [d]
  (let [s @world-state
        delta (dirs d)
        ;; mapv + adds two vectors
        new-coords (mapv + (get-in s [:entities :player :coords]) delta)]
    ;;update state only if new coords are on the grid
    (when (contains? (:grid s) new-coords)
      ;;update with new coords and coord change
      (swap! world-state update-in [:entities :player] assoc :coords new-coords :delta delta)
      ;;update fov using new info
      ;;and update world visbility based on fov
      ;;TODO put this in a general "move" fn
      (->
       (swap! world-state update-in [:entities :player] compute-fov)
       (get-in [:entities :player :fov])
       (update-vis!))
      ;;return move info
      {:move new-coords
       :time (if (get #{1 3 5 7} d) (get-in s [:entities :player :diag-time]) 10)}
      )))

;; if the player is on a closed box, open it and check for ananas
;;   returns box contents if a box was opened, otherwise nil
(defn open-box! []
  (let [s @world-state
        coords (get-in s [:entities :player :coords])]
    (when (= (get-in s [:grid coords]) :closed-box)
      (swap! world-state assoc-in [:grid coords] :open-box)
      ;; s is the old state, but that doesn't matter here
      (if (= (:ananas s) coords)
        (do (swap! world-state assoc :ananas [])
            {:ananas true})
        {:ananas false}))))