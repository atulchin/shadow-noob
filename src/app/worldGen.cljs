
(ns app.worldGen
  (:require ["rot-js" :as rot]
            [app.utils :as utils :refer [idiv split-int rotate-grid]]))

(defn- noise-fn []
  (let [n (rot/Noise.Simplex.)]
    (fn [x y scale]
      (.get n (/ x scale) (/ y scale)))))

(defonce noise2d (noise-fn))

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

;(def SPLIT-LIM 20)
(def SPLIT-LIM 40)
;(def LOT-W 5)
(def LOT-W 7)
;(def ROAD-W 2)
(def ROAD-W 4)

(defmethod split-zone :v [{[x y] :coords [w h] :dims}]
  (let [s (+ (quot w 4) (rand-int (quot w 2)))
        lim SPLIT-LIM
        ;;if v-block is narrow, make children that won't get horizontally split
        child-w (if (< h lim) :lot-e :h)
        child-e (if (< h lim) :lot-w :h)
        rw (min (max (inc (quot h 10)) ROAD-W) (* ROAD-W 2))]
    [{:type child-w :coords [x y] :dims [s h]}
     {:type :s-w :coords [(+ s x) y] :dims [1 h]}
     {:type :r-ns :coords [(+ s x 1) (dec y)] :dims [(- rw 2) (+ h 2)]}
     {:type :s-e :coords [(+ s x rw -1) y] :dims [1 h]}
     {:type child-e :coords [(+ s x rw) y] :dims [(- w s rw) h]}]))

(defmethod split-zone :h [{[x y] :coords [w h] :dims}]
  (let [s (+ (quot h 4) (rand-int (quot h 2)))
        lim SPLIT-LIM
        ;;if h-block is narrow, make children that won't get vertically split
        child-n (if (< w lim) :lot-s :v)
        child-s (if (< w lim) :lot-n :v)
        rw (min (max (inc (quot w 10)) ROAD-W) (* ROAD-W 2))]
    [{:type child-n :coords [x y] :dims [w s]}
     {:type :s-n :coords [x (+ s y)] :dims [w 1]}
     {:type :r-ew :coords [(dec x) (+ s y 1)] :dims [(+ w 2) (- rw 2)]}
     {:type :s-s :coords [x (+ s y rw -1)] :dims [w 1]}
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

(defn- fill-grid [v xs ys]
  (persistent! (reduce conj! (transient {}) (for [x xs y ys] [[x y] v]))))

(defn- clear-grid [m xs ys]
  (apply dissoc m (for [x xs y ys] [x y])))

(defn- fill-gridf [f xs ys]
  (persistent! (reduce conj! (transient {}) (for [x xs y ys] [[x y] (f x y)]))))

(defn- plot-tiles [[x y] [w h]]
  (let [bldg-h (min (max 4 (idiv h 3)) 9)
        yard-h (min (max 2 (idiv (- h bldg-h) 3)) 7)]
    {:grid
     (-> {}
         (into (fill-gridf #(if (< 0.6 (noise2d %1 %2 5)) :tree :empty)
                           (range x (+ x w)) (range y (+ y h))))
         (into (fill-grid :empty (range (inc x) (+ x w -1)) (range y (+ y yard-h -1))))
         (into (fill-gridf #(if (> 0.0 (noise2d %1 %2 10)) :flowers :empty)
                           (range (inc x) (+ x w -1)) (range (+ y yard-h -1) (+ y yard-h))))
         (into (fill-grid :path [(+ x (quot w 2))]  (range y (+ y yard-h))))
         (clear-grid (range (inc x) (+ x w -1)) (range (+ y yard-h) (+ y yard-h bldg-h))))
     :decor
     (-> {}
         (into (fill-grid :wall (range (inc x) (+ x w -1)) (range (+ y yard-h) (+ y yard-h bldg-h))))
         (assoc [(+ x (quot w 2)) (+ y yard-h)] :door
                [(+ x (quot w 2) (+ -1 (* 2 (rand-int 2)))) (dec y)] :can-spot))}))
(defn- road-tiles [[x y] [w h]]
  {:grid (-> {}
             (into (fill-grid :floor (range x (+ x w)) (range y (+ y h)))))
   :decor {}})
(defn- sidewalk-tiles [[x y] [w h]]
  {:grid (-> {}
             (into (fill-grid :path (range x (+ x w)) (range y (+ y h)))))
   :decor {}})

(defmulti zone-to-tiles :type)
(defmethod zone-to-tiles :default [_] nil)
(defmethod zone-to-tiles :r-ns [{:keys [coords dims]}] (road-tiles coords dims))
(defmethod zone-to-tiles :r-ew [{:keys [coords dims]}] (road-tiles coords dims))

(defmethod zone-to-tiles :s-n [{:keys [coords dims]}]
  (assoc-in (sidewalk-tiles coords dims) [:decor (mapv + coords [-1 0])] :street-light))
(defmethod zone-to-tiles :s-s [{:keys [coords dims]}]
  (assoc-in (sidewalk-tiles coords dims) [:decor (mapv + coords dims [0 -1])] :street-light))
(defmethod zone-to-tiles :s-w [{:keys [coords dims]}]
  (assoc-in (sidewalk-tiles coords dims) [:decor (mapv + coords [0 -1])] :street-light))
(defmethod zone-to-tiles :s-e [{:keys [coords dims]}]
  (assoc-in (sidewalk-tiles coords dims) [:decor (mapv + coords dims [-1 0])] :street-light))

(defmethod zone-to-tiles :plot-n [{:keys [coords dims]}] (plot-tiles coords dims))
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

(defn generate-town-grid [[w h]]
  (let [town (gen-zone-town [w h])
        roads (filter #(#{:r-ns :r-ew} (:type %)) town)
        non-road (remove #(#{:r-ns :r-ew} (:type %)) town)
        grid (time (apply merge-with into
                          (concat
                           (keep zone-to-tiles non-road)
                           ;;do roads last to overwite sidewalks
                           (keep zone-to-tiles roads))))]
    ;;wall shadows:
    (update grid :decor
            (fn [m] (into
                     (reduce-kv #(if (= %3 :wall)
                                   (assoc %1 (mapv + %2 [0 1]) :wall-s)
                                   %1) {} m)
                     m)))))

;; create a temporary mutable ref for the rot-js map generator to use
;;   then return an ordinary clj hashmap
(defn generate-dungeon-grid [[w h]]
  (let [rot-digger (rot/Map.Digger. w h)
        grid-ref (atom {:grid {} :decor {}})]
    (.create rot-digger (digger-callback grid-ref))
    ;(.create rot-digger)
    ;;for rooms, .create includes walls
    ;(doseq [r (.getRooms rot-digger)] (.create r (digger-callback grid-ref)))
    ;;but not for corridors
    ;(doseq [r (.getCorridors rot-digger)] (.create r (digger-callback grid-ref)))
    ;;wall shadows:
    (swap! grid-ref update :decor
           (fn [m] (into
                    (reduce-kv #(if (= %3 :wall)
                                  (assoc %1 (mapv + %2 [0 1]) :wall-s)
                                  %1) {} m)
                    m)))
    @grid-ref))

