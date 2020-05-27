
(ns app.world
  (:require ["rot-js" :as rot]
            [clojure.set :as set]
            [app.utils :as utils :refer [assoc-multi update-all sample]]))

(declare chase! compute-fov update-vis!)

;; just functions for querying and modifying world state
;;   probably shouldn't spawn any processes in here

;; mutable world state
;;   may be read/written concurrently by outside processes
;;   for entities, duplicate the hashkey inside the entity record
;;    -- this allows for cleaner code in some places & avoids searching
;;    :action is any function that gets called on the entity's turn
;;    -- it is not really an "act" method
(defonce world-state (atom {}))
(def init-state {:grid {}
                 :seen #{}
                 :visible {}
                 :entities {:player {:id :player :type :local-player
                                     :fov-fn :fov-360 :vision 10
                                     :move-time 10 :diag 1.4}
                            :pedro {:id :pedro :type :npc
                                    :fov-fn :fov-90 :vision 5
                                    :move-time 10 :diag 2.0
                                    :action #(chase! :pedro :player 4)}}
                 :effects #{}})

(defn reset-state [] (reset! world-state init-state))

;; effects that can be applied to entities
;;  format is [:entity-key updating-fn]
(def effects {:speed [:move-time #(* % 0.5)]})

;; active effects are listed in state's :effects field
;;  as [:entity entity-key :effect effect-key :end end-time]
(defn add-effect [state entity-key effect-key end-time]
  (update state :effects conj {:entity entity-key :effect effect-key :end end-time}))

;; return entity with effects applied
(defn get-entity [state key]
  (let [e-fx (map :effect (set/select #(= (:entity %) key) (:effects state)))]
    (reduce #(apply update %1 (get effects %2)) (get-in state [:entities key]) e-fx)
    ))

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
     (swap! world-state update :entities update-all #(assoc % :fov (compute-fov %)))
     (get-in [:entities :player :fov])
     (update-vis!))))

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
    (swap! data-ref assoc [x y] v)))

;;precise shadowcasting; 360 degrees only, but provides degrees of visibility
(defonce FOV-P (rot/FOV.PreciseShadowcasting. (light-callback)))

;;recursive shadowcasting; supports 90 and 180 degrees; binary visibility only
(defonce FOV-R (rot/FOV.RecursiveShadowcasting. (light-callback)))

;; fov computation: takes an entity record
;; multimehtod dispatch based on :fov-fn
(defmulti compute-fov #(:fov-fn %))

;;fov functions used by entities
;;  create a tmp mutable ref for callback fn, then return clj data struct
(defmethod compute-fov :fov-360 [{[x y] :coords vision :vision}]
  (let [data-ref (atom {})]
    (.compute FOV-P x y vision (fov-callback data-ref))
    @data-ref))

(defmethod compute-fov :fov-90 [{[x y] :coords vision :vision delta :delta}]
  (let [data-ref (atom {})
        dir (.indexOf dirs delta)]
    (.compute90 FOV-R x y vision dir (fov-callback data-ref))
    @data-ref))

;;updates world visibility info based on given fov data
(defn update-vis! [fovmap]
  (swap! world-state #(-> %
                          (assoc :visible fovmap)
                          (update :seen into (keys fovmap)))))

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
      (let [e (get-entity @world-state entity-key)]
        (swap! world-state assoc-in [:entities entity-key :fov] (compute-fov e))
        ;;return result
        {:path-length (count rest-of-path)
         :move next-step
         :dt (* (:move-time e)
                (if (get #{[1 1] [-1 -1] [1 -1] [-1 1]} delta) (:diag e) 1.0))}))
    
    ))

;; entity behavior: move towards target entity
;;   modifies entity coords in world state
;;   returns result of follow-path!
(defn chase! [entity-key target-key topology]
  (let [s @world-state
        e-coords (get-in s [:entities entity-key :coords])
        t-coords (get-in s [:entities target-key :coords])]
    (follow-path! entity-key (compute-path e-coords t-coords topology))))

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
      (let [e (get-entity @world-state :player)
            fov (compute-fov e)]
        (swap! world-state assoc-in [:entities :player :fov] fov)
        (update-vis! fov)
        ;;return move info
        {:move new-coords
         :dt (* (:move-time e)
                (if (get #{[1 1] [-1 -1] [1 -1] [-1 1]} delta) (:diag e) 1.0))})
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
