
(ns app.world
  (:require ["rot-js" :as rot]
            [clojure.set :as set]
            [app.utils :as utils :refer [assoc-multi update-all sample]]))

(declare chase! compute-fov update-vis)

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
                                     :move-time 10 :diag 1.4
                                     :inv {:potion-speed 1 :scroll-teleport 2}}
                            :pedro {:id :pedro :type :npc
                                    :fov-fn :fov-90 :vision 5
                                    :move-time 10 :diag 2.0
                                    :action #(chase! :pedro :player 4)}}
                 :time 0
                 :effects #{}
                 })

(defonce flow-fields (atom {}))

(defn reset-state [] 
  (reset! flow-fields {})
  (reset! world-state init-state))

(defn get-info [coords]
  (let [s @world-state]
    {:coords coords
     :time (:time s)
     :grid (get (:grid s) coords)
     :seen? (contains? (:seen s) coords)
     :visible? (contains? (:visible s) coords)
     :entities (map :id (filter #(= coords (:coords %))
                                (vals (:entities s))))}))

(defn update-player-field! [state]
  (swap! flow-fields assoc :player
            (utils/breadth-first #{(get-in state [:entities :player :coords])}
                                 (:grid state)
                                 utils/neigh8
                                 10)))

;; effects that can be applied to entities
;;  format is [:key updating-fn]
(def effects {:speed [:move-time #(* % 0.5)]})

;; active effects are listed in state's :effects field
;;  as [:entity entity-key :effect effect-key :end end-time]
(defn add-effect [state entity-key effect-key duration]
  (update state :effects conj
          {:entity entity-key :effect effect-key :end (+ (:time state) duration)}))

;; returns state with time t and expired effects removed
(defn set-time [s t]
  (merge s {:time t
            :effects (set/select #(> (:end %) t) (:effects s))}))

;; return entity with effects applied
(defn get-entity [state k]
  (let [e-fx (map :effect (set/select #(= (:entity %) k) (:effects state)))]
    (reduce #(apply update %1 (get effects %2)) (get-in state [:entities k]) e-fx)))

;; items
(defn add-item [state entity-key item-key]
  (update-in state [:entities entity-key :inv item-key] inc))

(defn remove-item [state entity-key item-key]
  (let [qty (dec (get-in state [:entities entity-key :inv item-key]))]
    (if (< qty 1)
      (update-in state [:entities entity-key :inv] dissoc item-key)
      (assoc-in state [:entities entity-key :inv item-key] qty))))

(defn teleport [state entity-key target-info]
  (let [target-key (first (:entities target-info))
        ;;if no target key, entity is target
        t (or target-key entity-key)
        ;; if no entity targeted but grid loc targeted, set location to targeted loc
        ;; otherwise, set location to random grid loc
        loc (if (and (nil? target-key) (:grid target-info))
              (:coords target-info)
              (rand-nth (keys (:grid state))))
        ;; change loc of target entity
        s' (assoc-in state [:entities t :coords] loc)
        fov (compute-fov (get-entity s' t))
        player-fov (compute-fov (get-entity s' :player))]
    ;;in case player moved:
    ;;TODO - move this
    (update-player-field! s')
    (-> s'
        ;;update entity's fov
        (assoc-in [:entities t :fov] fov)
        ;;update player's vision
        (update-vis player-fov))
    ))

;; item values are functions of world-state, entity-key, and
;;   target-info (as returned by get-info)
;; metadata {:target true} if item requires a target
(def items {:potion-speed (fn [state k _] (-> state
                                              (add-effect k :speed 100)
                                              (remove-item k :potion-speed)))
            :scroll-teleport ^:target (fn [state k t] (-> state
                                                          (teleport k t)
                                                          (remove-item k :scroll-teleport)))})

(defn use-item! [entity-key item-key target-info]
  (let [f (get items item-key)]
    (swap! world-state f entity-key target-info))
  ;;return a result
  {:item item-key :entity entity-key :dt 10})

(defn player-item! [item-key target-info]
  (use-item! :player item-key target-info))

;; grid directions are stored in rot-js as a javascript object
;;   convert to clj vector
(def dirs (get (js->clj (. rot -DIRS)) "8"))

;; rot-js map generator uses callback fn to modify an object
;;   this generates and returns that fn
(defn- digger-callback [grid-ref]
  (fn [x y v]
    (if (= v 1) ;;1 = wall
      (swap! grid-ref #(-> %
                           (assoc-in [:decor [x y]] :wall)
                           (update :grid dissoc [x y])
                           ))
      ;;store only passable spaces in :grid
      (swap! grid-ref assoc-in [:grid [x y]] :floor)))) 

;; create a temporary mutable ref for the rot-js map generator to use
;;   then return an ordinary clj hashmap
(defn- generate-grid [[w h]]
  (let [rot-digger (rot/Map.Digger. w h)
        ;noise (rot/Noise.Simplex.)
        grid-ref #_(atom {:grid {} :decor {}})
        (atom {:decor {}
               :grid {} #_(into {} (for [x (range (.-_width rot-digger))
                                    y (range (.-_height rot-digger))]
                                {[x y] 
                                 (if (< 0.6 (.get noise (/ x 3) (/ y 3))) :grass :empty)
                                 }))
               })]
    (.create rot-digger (digger-callback grid-ref))
    ;(.create rot-digger)
    ;;for rooms, .create includes walls
    ;(doseq [r (.getRooms rot-digger)] (.create r (digger-callback grid-ref)))
    ;;but not for corridors
    ;(doseq [r (.getCorridors rot-digger)] (.create r (digger-callback grid-ref)))
    ;;wall shadows:
    (swap! grid-ref update :decor
           (fn [m] (merge
                    (reduce #(assoc %1 (mapv + %2 [0 1]) :wall-s) {} (keys m))
                    m)))
    @grid-ref
    ))

;; create a starting map
(defn init-grid! [[w h]]
  (let [{:keys [grid decor]} (generate-grid [w h]) ;;initial map
        grid-keys (keys grid)
        box-keys (sample 5 grid-keys)  ;;random locations for boxes
        free-cells (set/difference (set grid-keys) (set box-keys)) ;;squares without boxes
        starting-entities (:entities @world-state)
        starting-coords (sample (count starting-entities) free-cells) ;;a vector of random starting locations
        ]
    (swap! world-state assoc
           :decor decor
           ;;add boxes to grid and store in :grid of world-state
           :grid (assoc-multi grid box-keys :closed-box)
           ;;give each starting entity a location (:coords key) and random facing
           ;;  an entity is a k-v pair where each v is a hashmap
           :entities (into {} (map (fn [[k v] x] [k (assoc v :coords x :facing (rand-int (count dirs)))])
                                   starting-entities starting-coords))
           ;;store location of boxes and ananas
           :boxes (vec box-keys) :ananas (first box-keys))
    ;; compute entities' fov based on starting info
    (swap! world-state update :entities update-all #(assoc % :fov (compute-fov %)))
    (swap! world-state #(update-vis % (get-in % [:entities :player :fov])))
    ;; initial flow-field to player
    (update-player-field! @world-state)
    ;; make a flow field for each box
    (swap! flow-fields merge
           (reduce
            #(assoc %1 %2 (utils/breadth-first #{%2} grid utils/neigh4 1000))
            {}
            box-keys))))

;; fov computation: takes an entity record
;; multimehtod dispatch based on :fov-fn
(defmulti compute-fov :fov-fn)

;;fov functions used by entities
(defmethod compute-fov :fov-360 [{[x y] :coords vision :vision}]
  ;; create a tmp mutable ref for callback fn, then return clj data struct
  ;; [does making a vector inside an atom transient do anything?]
  (let [data-ref (atom (transient {}))
        {:keys [grid]} @world-state
        ;;precise shadowcasting; 360 degrees only, but provides degrees of visibility
        ;; rot-js FOV needs a callback to determine if a cell is transparent
        FOV-P (rot/FOV.PreciseShadowcasting. #(contains? grid [%1 %2]))
        ;FOV-P (rot/FOV.RecursiveShadowcasting. #(contains? grid [%1 %2]))
        ]
    ;; rot-js FOV returns data using a callback w/ 4 args %1 and %2 = coords, %3 = distance, %4 = visibility
    (.compute FOV-P x y vision #(swap! data-ref assoc! [%1 %2] %4))
    (persistent! @data-ref)))

(defmethod compute-fov :fov-90 [{[x y] :coords vision :vision facing :facing}]
  ;; create a tmp mutable ref for callback fn, then return clj data struct
  ;; [does making a vector inside an atom transient do anything?]
  (let [data-ref (atom (transient {}))
        {:keys [grid]} @world-state
        ;;recursive shadowcasting; supports 90 and 180 degrees; binary visibility only
        ;; rot-js FOV needs a callback to determine if a cell is transparent
        FOV-R (rot/FOV.RecursiveShadowcasting. #(contains? grid [%1 %2]))]
    ;; rot-js FOV returns data using a callback w/ 4 args %1 and %2 = coords, %3 = distance, %4 = visibility
    (.compute90 FOV-R x y vision facing #(swap! data-ref assoc! [%1 %2] %4))
    (persistent! @data-ref)))

;;for updating world visibility info based on given fov data
(defn update-vis [state fovmap]
  (-> state
      (assoc :visible fovmap)
      (update :seen into (keys fovmap))))

;; start and destination are coord pairs
;;   topo = # of movement directions
#_(defn- compute-path [[start-x start-y] [dest-x dest-y] topo]
    (let [{:keys [grid]} @world-state
        ;; rot-js pathfinder needs a callback fn to determine if a cell is passable
          astar (rot/Path.AStar. dest-x dest-y #(contains? grid [%1 %2]) #js{:topology topo})
        ;; create a temporary mutable ref for rot-js pathfinder to use
        ;;  [does making a vector inside an atom transient do anything?]
          path-ref (atom (transient []))]
    ;; rot-js pathfinder builds a path using a callback fn to mutate an object
      (.compute astar start-x start-y #(swap! path-ref conj! [%1 %2]))
    ;; return an ordinary clj vector
      (persistent! @path-ref)))

;; mutate the :coords of the provided entity within world-state
;; path is a vector of coordinate pairs
;;   rot-js pathfinder includes the starting point as the first item in the path
;; returns remanining path length
#_(defn- follow-path! [entity-key [start-coords next-step & rest-of-path]]
    (if next-step
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
    ;;else entity is already at end of path
      {:path-length 0 :dt 0}))

;; return entity with updated info
(defn move-entity [e coords]
  (let [delta (mapv - coords (:coords e))
        idx-delta (.indexOf dirs delta)
        e' (assoc e :coords coords :delta delta
                  ;;if delta is in dirs, change facing
                  :facing (if (contains? dirs idx-delta) idx-delta (:facing e)))]
    ;;return record with updated fov
    (assoc e' :fov (compute-fov e'))))

;; calcualtes time taken by entity's last move
(defn- move-time [e]
  (* (:move-time e)
     (if (get #{[1 1] [-1 -1] [1 -1] [-1 1]} (:delta e)) (:diag e) 1.0)))

(defn- move-random! [entity-key topo]
  (let [s @world-state
        neigh-fn (if (= 4 topo) utils/neigh4 utils/neigh8)
        coords (get-in s [:entities entity-key :coords])]
    (when-let [next-step (rand-nth (neigh-fn (:grid s) coords))]
      (let [s' (swap! world-state
                      update-in [:entities entity-key] move-entity next-step)]
        ;;return result (using get-entity for entity with effects)
        {:wander next-step :dt (move-time (get-entity s' entity-key))}
        ))))

(defn- follow-field! [entity-key field topo]
  (let [neigh-fn (if (= 4 topo) utils/neigh4 utils/neigh8)
        coords (get-in @world-state [:entities entity-key :coords])]
    ;;find the lowest-valued entry in field among neighbors of coords
    ;;if coords have no neightbors in field, return nil
    (when-let [next-step (apply min-key field (neigh-fn field coords))]
      ;; check if entity is already at field minimum
      (if (and (get field coords) (< (get field coords) (get field next-step)))
        {:path-length 0 :dt 0}
      ;; else update state
        (let [s' (swap! world-state
                        update-in [:entities entity-key] move-entity next-step)]
          ;;return result (using get-entity for entity with effects)
          {:path-length (get field next-step) 
           :move next-step 
           :dt (move-time (get-entity s' entity-key))})
        ))
    ))

(defn- patrol-boxes! [entity-key topo]
  (let [s @world-state
        f @flow-fields
        e (get-in s [:entities entity-key])
        coords (:coords e)
        neigh-fn (if (= 4 topo) utils/neigh4 utils/neigh8)
        ;;if entity's to do list is empty, get all boxes from world
        box-todo (or (seq (:box-todo e)) (:boxes s))
        ;;min all fields of boxes on to do list
        field (reduce #(merge-with min %1 (get f %2)) {} box-todo)]
    ;; TODO - generalize the follow-field function
    (when-let [next-step (apply min-key field (neigh-fn field coords))]
      ;; check if entity is already at field minimum
      (if (and (get field coords) (< (get field coords) (get field next-step)))
        (do ;;remove box from to do list
          (swap! world-state
                 assoc-in [:entities entity-key :box-todo]
                 (disj (set box-todo) coords))
          {:checking-box coords :dt 10})
      ;; else update coords
        (let [s' (swap! world-state
                        update-in [:entities entity-key] move-entity next-step)]
          ;;return result (using get-entity for entity with effects)
          {:patrolling (get field next-step)
           :move next-step
           :dt (move-time (get-entity s' entity-key))})))
    ))


#_(defn chase! [entity-key target-key topology]
    (let [s @world-state
          e-coords (get-in s [:entities entity-key :coords])
          t-coords (get-in s [:entities target-key :coords])]
      (follow-path! entity-key (compute-path e-coords t-coords topology))))
(defn chase! [entity-key target-key topology]
  (or (follow-field! entity-key (get @flow-fields target-key) topology)
      (patrol-boxes! entity-key topology)
      (move-random! entity-key topology)))


;; moves player in a given grid direction
;;   updates world state
;;   returns result if player was moved, otherwise nil
(defn move-player! [d]
  (let [s @world-state
        player-info (get-in s [:entities :player])
        new-coords (mapv + (:coords player-info) (dirs d))]
    ;;update state only if new coords are on the grid
    (when (contains? (:grid s) new-coords)
      (let [updated-info (move-entity player-info new-coords)
            ;; update player info and world vis map
            s' (swap! world-state
                      #(-> %
                           (assoc-in [:entities :player] updated-info)
                           (update-vis (:fov updated-info))))]
        ;;player coords changed, so update flow field
        (update-player-field! s')
        ;;return move info, using entity with effects applied
        {:move new-coords :dt (move-time (get-entity s' :player))}
        ))
    ))

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
