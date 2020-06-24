
(ns app.world
  (:require ["rot-js" :as rot]
            [clojure.set :as set]
            [app.utils :as utils :refer 
             [assoc-multi update-all sample 
              merge2 mergef tmerge split-int rotate-grid]])
  )

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
                 ;:light {}
                 :light-sources {}
                 :entities {:player {:id :player :type :local-player
                                     :fov-fn :fov-player :vision 6
                                     :move-time 10 :diag 1.4
                                     :inv {:potion-speed 1 :scroll-teleport 2}}
                            :pedro {:id :pedro :type :npc
                                    :fov-fn :fov-90 :vision 5
                                    :move-time 10 :diag 2.0
                                    :action #(chase! :pedro :player 4)}}
                 :time 0
                 :effects #{}})

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
    ;(update-player-field! s')
    (-> s'
        ;;update entity's fov
        (assoc-in [:entities t :fov] fov)
        ;;update player's vision
        (update-vis player-fov))))

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

(def things {:tree {:blocks-light true}})

(defn transparent? [grid coords]
  (let [v (get grid coords)]
    (and v (not (get-in things [v :blocks-light])))))

;; rot-js map generator uses callback fn to modify an object
;;   this generates and returns that fn
(defn- digger-callback [grid-ref]
  (fn [x y v]
    (if (= v 1) ;;1 = wall
      (swap! grid-ref #(-> %
                           (assoc-in [:decor [x y]] :wall)
                           (update :grid dissoc [x y])))
      ;;store only passable spaces in :grid
      (swap! grid-ref assoc-in [:grid [x y]] :floor))))

(defmulti split-zone :type)

(def SPLIT-LIM 20)
;(def SPLIT-LIM 40)
(def LOT-W 5)
;(def LOT-W 7)
(def ROAD-W 2)
;(def ROAD-W 4)

(defmethod split-zone :v [{[x y] :coords [w h] :dims}]
  (let [s (+ (quot w 4) (rand-int (quot w 2)))
        lim SPLIT-LIM
        ;;if v-block is narrow, make children that won't get horizontally split
        child-w (if (< h lim) :lot-e :h)
        child-e (if (< h lim) :lot-w :h)
        rw (min (max (inc (quot h 10)) ROAD-W) (* ROAD-W 2))]
    [{:type child-w :coords [x y] :dims [s h]}
     {:type :r :coords [(+ s x) y] :dims [rw h]}
     {:type child-e :coords [(+ s x rw) y] :dims [(- w s rw) h]}]))

(defmethod split-zone :h [{[x y] :coords [w h] :dims}]
  (let [s (+ (quot h 4) (rand-int (quot h 2)))
        lim SPLIT-LIM
        ;;if h-block is narrow, make children that won't get vertically split
        child-n (if (< w lim) :lot-s :v)
        child-s (if (< w lim) :lot-n :v)
        rw (min (max (inc (quot w 10)) ROAD-W) (* ROAD-W 2))]
    [{:type child-n :coords [x y] :dims [w s]}
     {:type :r :coords [x (+ s y)] :dims [w rw]}
     {:type child-s :coords [x (+ s y rw)] :dims [w (- h s rw)]}]))

(defmethod split-zone :lot-s [{[x y] :coords [w h] :dims}]
  (let [n (quot w LOT-W)
        widths (split-int w n)
        s-pts (into [0] (reductions + widths))]
    (for [i (range n)]
      {:type :plot-s :coords [(+ (s-pts i) x) y] :dims [(widths i) h]})))

(defmethod split-zone :lot-n [{[x y] :coords [w h] :dims}]
  (let [n (quot w LOT-W)
        widths (split-int w n)
        s-pts (into [0] (reductions + widths))]
    (for [i (range n)]
      {:type :plot-n :coords [(+ (s-pts i) x) y] :dims [(widths i) h]})))

(defmethod split-zone :lot-w [{[x y] :coords [w h] :dims}]
  (let [n (quot h LOT-W)
        widths (split-int h n)
        s-pts (into [0] (reductions + widths))]
    (for [i (range n)]
      {:type :plot-w :coords [x (+ (s-pts i) y)] :dims [w (widths i)]})))

(defmethod split-zone :lot-e [{[x y] :coords [w h] :dims}]
  (let [n (quot h LOT-W)
        widths (split-int h n)
        s-pts (into [0] (reductions + widths))]
    (for [i (range n)]
      {:type :plot-e :coords [x (+ (s-pts i) y)] :dims [w (widths i)]})))

(defn fill-grid [v xs ys]
  (persistent! (reduce conj! (transient {}) (for [x xs y ys] [[x y] v]))))

(defn clear-grid [m xs ys]
  (apply dissoc m (for [x xs y ys] [x y])))

(defn fill-gridf [f xs ys]
  (persistent! (reduce conj! (transient {}) (for [x xs y ys] [[x y] (f x y)])))
  )

(defonce noise (rot/Noise.Simplex.))

(defn plot-tiles [[x y] [w h]]
  (let [bldg-h 3 #_(max 2 (min (dec w) (- h 4)))]
    {:grid (->
            (fill-gridf #(if (< 0.6 (.get noise (/ %1 3) (/ %2 3))) :tree :empty)
                        (range x (+ x w)) (range y (+ y h)))
            (tmerge (fill-grid :empty (range (inc x) (+ x w -1)) (range y (+ y 2)))
                    (fill-grid :path [(+ x (quot w 2))]  (range y (+ y 2)))
                    )
            (clear-grid (range (inc x) (+ x w -1)) (range (+ y 2) (+ y 2 bldg-h)))
            )
     :decor
     (-> 
      (fill-grid :wall (range (inc x) (+ x w -1)) (range (+ y 2) (+ y 2 bldg-h)))
      (assoc [(+ x (quot w 2)) (+ y 2)] :door)
      )
     }
    ))

(defmulti zone-to-tiles :type)
(defmethod zone-to-tiles :default [_] nil)
(defmethod zone-to-tiles :r [{:keys [coords dims]}]
  (let [[x y] coords [w h] dims]
    {:grid (fill-grid :floor (range x (+ x w)) (range y (+ y h)))}
    ))
(defmethod zone-to-tiles :plot-n [{:keys [coords dims]}]
  (plot-tiles coords dims))
(defmethod zone-to-tiles :plot-s [{:keys [coords dims]}]
  ;; k is :grid or :decor, v is coord map
  (reduce-kv #(assoc %1 %2 (rotate-grid 180 coords dims %3)) {} (plot-tiles coords dims)))
(defmethod zone-to-tiles :plot-w [{:keys [coords dims]}]
  (let [[w h] dims]
    (reduce-kv #(assoc %1 %2 (rotate-grid 270 coords [h w] %3)) {} (plot-tiles coords [h w]))))
(defmethod zone-to-tiles :plot-e [{:keys [coords dims]}]
  (let [[w h] dims]
    (reduce-kv #(assoc %1 %2 (rotate-grid 90 coords [h w] %3)) {} (plot-tiles coords [h w]))))

(defn- gen-zone-town [dims]
  (tree-seq #(#{:v :h :lot-n :lot-s :lot-e :lot-w} (:type %))
            #(split-zone %)
            {:type :v :coords [0 0] :dims dims}))

;; create a temporary mutable ref for the rot-js map generator to use
;;   then return an ordinary clj hashmap
(defn- generate-grid [[w h]]
  (let [rot-digger (rot/Map.Digger. w h)
        town (time (gen-zone-town [w h]))
        bg-grass {} #_(into {} (for [x (range w) y (range h)]
                                 [[x y]
                            ;(if (< 0.6 (.get noise (/ x 3) (/ y 3))) :grass :empty)
                                  :empty]))
        grid-ref #_(atom {:grid {} :decor {}})
        (atom (time (apply merge-with merge (keep zone-to-tiles town))) 
         )]
    ;(.create rot-digger (digger-callback grid-ref))
    ;(.create rot-digger)
    ;;for rooms, .create includes walls
    ;(doseq [r (.getRooms rot-digger)] (.create r (digger-callback grid-ref)))
    ;;but not for corridors
    ;(doseq [r (.getCorridors rot-digger)] (.create r (digger-callback grid-ref)))
    ;;wall shadows:
    #_(swap! grid-ref update :decor
             (fn [m] (merge
                      (reduce #(assoc %1 (mapv + %2 [0 1]) :wall-s) {} (keys m))
                      m)))
    @grid-ref))

(defn compute-light [grid light-sources]
 (let [light-fov (rot/FOV.PreciseShadowcasting.
                  #(transparent? grid [%1 %2]) 
                  ;#js{:topology 8}
                  )
       light (rot/Lighting. 
              nil;#(if (contains? grid [%1 %2]) 0.5 0) 
              #js{:range 6 :passes 1}
              )
       data-ref (atom {})]
   (.setFOV light light-fov)
   (doseq [[[x y] v] light-sources] (.setLight light x y (clj->js v)))
   (.compute light #(swap! data-ref assoc [%1 %2] (js->clj %3)))
   @data-ref))

;; fov map with coord keys and distance values
(defn fovmax [grid [x y]]
  (let [data-ref (atom {})
        MAX-DIST 60
        FOV-R (rot/FOV.RecursiveShadowcasting. #(transparent? grid [%1 %2]))]
    (.compute FOV-R x y MAX-DIST #(swap! data-ref assoc [%1 %2] %3))
    @data-ref))

;; fov computation: takes an entity record
;; multimehtod dispatch based on :fov-fn
(defmulti compute-fov :fov-fn)

;;fov functions used by entities
;; player fov is a convenient time to update world light map
;; and sight-map to player
(defmethod compute-fov :fov-player [{[x y] :coords vision :vision}]
  ;; create a tmp mutable ref for callback fn, then return clj data struct
  (let [{:keys [grid light-sources]} @world-state
        vismap (compute-light grid
                              ;; player emits neutral light for fov and shadow effect
                              (assoc light-sources [x y] [128 128 128]))
        ;;fov map with distance
        fov (fovmax grid [x y])]

    (swap! world-state assoc :light vismap)
    (swap! flow-fields assoc :player fov)
    (select-keys vismap (keys fov))
    ))

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
                                            m)))
                                 ))
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
    ;; compute lighting
    (swap! world-state assoc :light-sources (zipmap box-keys (repeat [255 200 0])))
    #_(swap! world-state #(assoc % :light
                               (compute-light
                                (:grid %)
                                (zipmap box-keys (repeatedly rand-color)))))    
    ;; compute entities' fov based on starting info
    (swap! world-state update :entities update-all #(assoc % :fov (compute-fov %)))
    (swap! world-state #(update-vis % (get-in % [:entities :player :fov])))
    ;; initial flow-field to player
    ;(update-player-field! @world-state)
    ;; make a flow field for each box
    (swap! flow-fields merge
           (reduce
            #(assoc %1 %2 (utils/breadth-first #{%2} grid utils/neigh4 50))
            {}
            box-keys))))


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
        {:wander next-step :dt (move-time (get-entity s' entity-key))}))))

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
           :dt (move-time (get-entity s' entity-key))})))))

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
        #_(reduce #(merge-with min %1 (get f %2)) {} box-todo)
        (reduce #(merge2 min %1 (get f %2))
                (get f (first box-todo)) (rest box-todo))
        #_(time (mergef min (map #(get f %) box-todo)))]
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
           :dt (move-time (get-entity s' entity-key))})))))


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
        ;(update-player-field! s')
        ;;return move info, using entity with effects applied
        {:move new-coords :dt (move-time (get-entity s' :player))}))))

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
