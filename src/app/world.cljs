
(ns app.world
  (:require ["rot-js" :as rot]
            [clojure.set :as set]
            [app.worldGen :as worldGen :refer [generate-town-grid]]
            [app.utils :as utils :refer [assoc-multi update-all sample merge2 mergef]]))

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
                 :decor {}
                 :obj {}
                 :items {}
                 :seen #{}
                 :visible {}
                 ;:light {}
                 :light-sources {}
                 :entities {:player {:id :player :type :local-player
                                     :fov-fn :fov-player :vision 6
                                     :move-time 10 :diag 1.4
                                     :inv {:potion-speed 1 :scroll-teleport 2}}
                            :pedro {:id :pedro :type :npc
                                    :fov-fn :fov-90 :vision 5
                                    :move-time 10 :diag 2.0
                                    :action #(chase! :pedro :player 10 4)}}
                 :time 0
                 :status {}
                 :effects #{}})

(defonce flow-fields (atom {}))

(defn reset-state []
  (reset! flow-fields {})
  (reset! world-state init-state))

(defn- coord-info [s coords]
  {:coords coords
   :time (:time s)
   :grid (get (:grid s) coords)
   :obj (get (:obj s) coords)
   :seen? (contains? (:seen s) coords)
   :visible? (contains? (:visible s) coords)
   :entities (map :id (filter #(= coords (:coords %))
                              (vals (:entities s))))})

(defn get-info [coords] (coord-info @world-state coords))

(defn player-coord-info []
  (let [s @world-state]
    (coord-info s (get-in s [:entities :player :coords]))))

#_(defn update-player-field! [state]
    (swap! flow-fields assoc :player
           {} #_(utils/breadth-first #{(get-in state [:entities :player :coords])}
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
  (into s {:time t
           :effects (set/select #(> (:end %) t) (:effects s))}))

(defn- get-entfx [state k]
  (map :effect (set/select #(= (:entity %) k) (:effects state))))

;; return entity with effects applied
(defn entity-with-effects [state k]
  (let [e-fx (get-entfx state k)]
    ;;key and fn are returned by (get effects), apply flattens them into args to update
    (reduce #(apply update %1 (get effects %2)) (get-in state [:entities k]) e-fx)))

(defn apply-effects [state entity-record]
  (let [e-fx (get-entfx state (:id entity-record))]
    ;;key and fn are returned by (get effects), apply flattens them into args to update
    (reduce #(apply update %1 (get effects %2)) entity-record e-fx)))

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
        fov (compute-fov (entity-with-effects s' t))
        player-fov (compute-fov (entity-with-effects s' :player))]
    ;;in case player moved:
    ;;TODO - move this
    ;(update-player-field! s')
    (-> s'
        ;;update entity's fov
        (assoc-in [:entities t :fov] fov)
        ;;update player's vision
        (update-vis player-fov))))

;; if the player interacts with a closed box, open it and check for ananas
;;   returns new state if a box was opened, otherwise nil
(defn open-box [s ent-key targ-m]
  (let [coords (:coords targ-m)
        ananas-coords (:ananas s)
        ananas? (= coords ananas-coords)]
    (when (= (:obj targ-m) :closed-box)
      (-> s
          (assoc-in [:obj coords] :open-box)
          (assoc :ananas (if ananas? [] ananas-coords))
          (assoc :status {:ananas ananas? :dt 10}))
      )))

;; item values are functions of world-state, entity-key, and
;;   target-info (as returned by get-info)
;; metadata {:target true} if item requires a target
(def item-fns
  {:potion-speed {:drink
                  (fn [state k _] (-> state
                                      (add-effect k :speed 100)
                                      (remove-item k :potion-speed)
                                      (assoc :status {:entity k :drink :potion-speed :dt 10})
                                      ))}
   :scroll-teleport {:read ^:target
                     (fn [state k t] (-> state
                                         (teleport k t)
                                         (remove-item k :scroll-teleport)
                                         (assoc :status {:entity k :read :scroll-teleport :dt 10})
                                         ))}
   :closed-box {:open (fn [s k t] (open-box s k t))}
   })

(defn interact! [entity-key obj verb targ-m]
  (when-let [f (get-in item-fns [obj verb])]
    (when-let [s' (f @world-state entity-key targ-m)]
      (reset! world-state s')
      ;;return status
      (:status s')
      )))

(defn player-interact! [obj verb targ-m]
  (interact! :player obj verb targ-m))

;; grid directions are stored in rot-js as a javascript object
;;   convert to clj vector
(def dirs (get (js->clj (. rot -DIRS)) "8"))

(def light-blockers #{:tree})

(defn transparent? [grid coords]
  (let [v (get grid coords)]
    (and v (not (contains? light-blockers v)))))

(defn compute-light [grid dist light-sources]
  (let [light-fov (rot/FOV.PreciseShadowcasting.
                  ;;squares with light source are automatically transparent
                   #(or (transparent? grid [%1 %2]) (contains? light-sources [%1 %2]))
                  ;#js{:topology 8}
                   )
        light (rot/Lighting.
               nil;#(if (contains? grid [%1 %2]) 0.5 0) 
               #js{:range (inc dist) :passes 1})
        data-ref (atom {})
       ;;recursive fov for shaping the light
        FOV-R (rot/FOV.RecursiveShadowcasting. #(transparent? grid [%1 %2]))
       ;;light sources are auto included
        fov-ref (atom (set (keys light-sources)))]
    (.setFOV light light-fov)
   ;;light sources are [coords rgb-vector]
    (doseq [[[x y] v] light-sources]
      (.setLight light x y (clj->js v))
     ;;add this light source's fov to "lightable" squares
      (.compute FOV-R x y dist #(swap! fov-ref conj [%1 %2])))
   ;;compute light map
    (let [fov @fov-ref]
      (.compute light #(when (contains? fov [%1 %2])
                         (swap! data-ref assoc [%1 %2] (js->clj %3)))))
   ;;this makes square light grids
   ;(.compute light #(swap! data-ref assoc [%1 %2] (js->clj %3)))
    @data-ref))

;; fov map with coord keys and distance values
(defn fovmax [grid [x y]]
  (let [data-ref (atom {})
        MAX-DIST 60
        FOV-R (rot/FOV.RecursiveShadowcasting. #(transparent? grid [%1 %2]))]
    (.compute FOV-R x y MAX-DIST #(swap! data-ref assoc [%1 %2] %3))
    @data-ref))


;; TODO -- should pass state to compute-fov instead of derefing inside

;; fov computation: takes an entity record
;; multimehtod dispatch based on :fov-fn
(defmulti compute-fov :fov-fn)

;;fov functions used by entities
;; player fov is a convenient time to update world light map
;; and sight-map to player
(defmethod compute-fov :fov-player [{[x y] :coords vision :vision}]
  ;; create a tmp mutable ref for callback fn, then return clj data struct
  (let [{:keys [grid light]} @world-state
        vismap (mergef
                #(mapv max %1 %2)
                ;; player emits neutral light for fov and shadow effect
                (compute-light grid vision {[x y] [96 96 96]})
                ;; using static precomputed light map
                light)
        ;;fov map with distance
        fov (fovmax grid [x y])]

    ;;sight-map to player = fov dists whose coords are in the grid
    (swap! flow-fields assoc :player (select-keys fov (keys grid)))
    ;;actual fov = squares with light and line of sight
    (select-keys vismap (keys fov))))

#_(defmethod compute-fov :fov-360 [{[x y] :coords vision :vision}]
  ;; create a tmp mutable ref for callback fn, then return clj data struct
    (let [data-ref (atom {})
          MAX-DIST 60
          {:keys [grid light]} @world-state
        ;;precise shadowcasting; 360 degrees only, but provides degrees of visibility
        ;; rot-js FOV needs a callback to determine if a cell is transparent
          FOV-P (rot/FOV.PreciseShadowcasting. #(or (transparent? grid [%1 %2]) (= [x y] [%1 %2])))
        ;;recursive shadowcasting; better features
          FOV-R (rot/FOV.RecursiveShadowcasting. #(transparent? grid [%1 %2]))]
    ;; rot-js FOV returns data using a callback w/ 4 args %1 and %2 = coords, %3 = distance, %4 = visibility
      (.compute FOV-R x y vision #(swap! data-ref assoc [%1 %2] %4))
    ;; if it's in the light, it's in fov even if past vision range
      (.compute FOV-R x y MAX-DIST #(when (contains? light [%1 %2])
                                      (swap! data-ref assoc [%1 %2] %4)))
    ;;use precise for shadow info
      (.compute FOV-P x y vision (fn [x y r v]
                                   (swap! data-ref
                                          (fn [m]
                                            (if (contains? m [x y])
                                              (assoc m [x y] v)
                                              m)))))
      (identity @data-ref)))

(defmethod compute-fov :fov-90 [{[x y] :coords vision :vision facing :facing}]
  ;; create a tmp mutable ref for callback fn, then return clj data struct
  ;; [does making a vector inside an atom transient do anything?]
  (let [data-ref (atom (transient {}))
        {:keys [grid]} @world-state
        ;;recursive shadowcasting; supports 90 and 180 degrees; binary visibility only
        ;; rot-js FOV needs a callback to determine if a cell is transparent
        FOV-R (rot/FOV.RecursiveShadowcasting. #(transparent? grid [%1 %2]))]
    ;; rot-js FOV returns data using a callback w/ 4 args %1 and %2 = coords, %3 = distance, %4 = visibility
    (.compute90 FOV-R x y vision facing #(swap! data-ref assoc! [%1 %2] %4))
    (persistent! @data-ref)))

;;for updating world visibility info based on given fov data
(defn update-vis [state fovmap]
  (-> state
      (assoc :visible fovmap)
      (update :seen into (keys fovmap))))

;; create a starting map
(defn init-grid! [[w h]]
  (let [{:keys [grid decor]} (generate-town-grid [w h]) ;;initial map
        grid-keys (keys grid)
        can-spots (keys (filter (comp #{:can-spot} val) decor))
        box-keys (sample 5 grid-keys)  ;;random locations for boxes
        free-cells (set/difference (set grid-keys) (set box-keys)) ;;squares without boxes
        starting-entities (:entities @world-state)
        starting-coords (sample (count starting-entities) free-cells) ;;a vector of random starting locations
        light-sources (zipmap
                       (keys (filter (comp #{:street-light} val) decor))
                       (repeat [320 240 0]))]
    (swap! world-state assoc
           :grid grid
           :decor decor
           ;;add boxes to grid and store in :grid of world-state
           :obj (assoc-multi {} box-keys :closed-box)
           ;;give each starting entity a location (:coords key) and random facing
           ;;  an entity is a k-v pair where each v is a hashmap
           :entities (into {} (map (fn [[k v] x] [k (assoc v :coords x :facing (rand-int (count dirs)))])
                                   starting-entities starting-coords))
           ;;store location of boxes and ananas
           :boxes (vec box-keys) :ananas (first box-keys)
           :light-sources light-sources
           ;; compute lighting
           :light (compute-light grid 4 light-sources))
    ;; compute entities' fov based on starting info
    (swap! world-state update :entities update-all #(assoc % :fov (compute-fov %)))
    (swap! world-state #(update-vis % (get-in % [:entities :player :fov])))
    ;; initial flow-field to player
    ;(update-player-field! @world-state)
    ;; make a flow field for each box
    (swap! flow-fields into
           (reduce
            #(assoc %1 %2 (utils/breadth-first #{%2} grid utils/neigh4 50))
            {}
            box-keys))))

;; return entity with updated info
(defn move-entity [e coords]
  (let [delta (mapv - coords (:coords e))
        idx-delta (.indexOf dirs delta)
        e' (assoc e :coords coords :delta delta
                  ;;if delta is in dirs, change facing
                  :facing (if (contains? dirs idx-delta) idx-delta (:facing e)))]
    ;;return record with updated fov
    ;;TODO -- not using effects for fov computation
    (assoc e' :fov (compute-fov e'))))

;; calcualtes time taken by entity's last move
(defn- move-time [e]
  (* (:move-time e)
     (if (get #{[1 1] [-1 -1] [1 -1] [-1 1]} (:delta e)) (:diag e) 1.0)))

;; moves player in a given grid direction
;;   returns updated world state if player was moved, otherwise nil
#_(defn move-player [s d]
  (let [player-record (get-in s [:entities :player])
        new-coords (mapv + (:coords player-record) (dirs d))]
    ;;update state only if new coords are on the grid
    (when (contains? (:grid s) new-coords)
      (let [updated-player (move-entity player-record new-coords)]
        ;; update player info and world vis map
        (-> s
            (assoc-in [:entities :player] updated-player)
            (update-vis (:fov updated-player))
            (assoc :status {:move new-coords :dt (move-time (apply-effects s updated-player))}))
        ))))

#_(defn move-player! [d]
  (when-let [s' (move-player @world-state d)]
    (reset! world-state s')
    ;;player coords changed, so update flow field
    ;(update-player-field! s')
    ;;return status
    (:status s')))

(defn move-player! [d]
  (let [s @world-state
        player-info (get-in s [:entities :player])
        new-coords (mapv + (:coords player-info) (dirs d))]
    ;;update state only if new coords are on the grid
    (when (contains? (:grid s) new-coords)
      (let [updated-info (move-entity player-info new-coords)
            dt (move-time (apply-effects s updated-info))
            ;; update player info and world vis map
            s' (swap! world-state
                      #(-> %
                           (assoc-in [:entities :player] updated-info)
                           (update-vis (:fov updated-info))
                           (assoc :status {:move new-coords :dt dt})
                           ))]
        ;;player coords changed, so update flow field
        ;(update-player-field! s')
        ;;return status
        (:status s')
        ))))

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

(defn- move-random! [entity-key topo]
  (let [s @world-state
        neigh-fn (if (= 4 topo) utils/neigh4 utils/neigh8)
        coords (get-in s [:entities entity-key :coords])]
    (when-let [next-step (rand-nth (neigh-fn (:grid s) coords))]
      (let [s' (swap! world-state
                      update-in [:entities entity-key] move-entity next-step)]
        ;;return result (using get-entity for entity with effects)
        {:wander next-step :dt (move-time (entity-with-effects s' entity-key))}))))

(defn- follow-field! [entity-key field dist topo]
  (let [neigh-fn (if (= 4 topo) utils/neigh4 utils/neigh8)
        coords (get-in @world-state [:entities entity-key :coords])
        curr-val (get field coords)]
    ;;find the lowest-valued entry in field among neighbors of coords
    ;;if coords have no neightbors in field, return nil
    (when-let [next-step (and curr-val
                              (< curr-val dist)
                              (apply min-key field (neigh-fn field coords)))]
      ;; check if entity is already at field minimum
      (if (and curr-val (< curr-val (get field next-step)))
        {:path-length 0 :dt 0}
      ;; else update state
        (let [s' (swap! world-state
                        update-in [:entities entity-key] move-entity next-step)]
          ;;return result (using get-entity for entity with effects)
          {:path-length (get field next-step)
           :move next-step
           :dt (move-time (entity-with-effects s' entity-key))})))))

(defn- patrol-boxes! [entity-key topo]
  (let [s @world-state
        f @flow-fields
        e (get-in s [:entities entity-key])
        coords (:coords e)
        neigh-fn (if (= 4 topo) utils/neigh4 utils/neigh8)
        ;;if entity's to do list is empty, get all boxes from world
        box-todo (or (seq (:box-todo e)) (:boxes s))
        ;;min all fields of boxes on to do list
        field
        (reduce #(merge2 min %1 (get f %2))
                (get f (first box-todo)) (rest box-todo))]
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
           :dt (move-time (entity-with-effects s' entity-key))})))))


#_(defn chase! [entity-key target-key topology]
    (let [s @world-state
          e-coords (get-in s [:entities entity-key :coords])
          t-coords (get-in s [:entities target-key :coords])]
      (follow-path! entity-key (compute-path e-coords t-coords topology))))
(defn chase! [entity-key target-key dist topology]
  (or (follow-field! entity-key (get @flow-fields target-key) dist topology)
      (patrol-boxes! entity-key topology)
      (move-random! entity-key topology)))
