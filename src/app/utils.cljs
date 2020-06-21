
(ns app.utils)

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

(defn split-int [num parts]
  (loop [w num n parts s []]
    (let [x (quot w n)
          s' (conj s x)]
      (if (< n 2)
        s'
        (recur (- w x) (dec n) s'))
      )))

;; transients versions of merge-with
(defn merge2 [f a b]
  (persistent!
   (reduce
    (fn [m [k v]]
      (if-let [v0 (get m k)]
        (assoc! m k (f v0 v))
        (assoc! m k v)))
    (transient a)
    b)))

(defn mergef [f maps]
  (persistent!
   (reduce
    (fn [m-tr m2]
      (reduce (fn [m [k v]]
                (if-let [v0 (get m k)]
                  (assoc! m k (f v0 v))
                  (assoc! m k v))) m-tr m2))
    (transient (first maps))
    (rest maps))))

(defn tmerge [& maps]
  (persistent!
   (reduce
    (fn [m-tr m2]
      (reduce (fn [m [k v]] (assoc! m k v)) m-tr m2))
    (transient (first maps))
    (rest maps))))

(defn neigh4 [grid coords]
  (filter grid (map #(mapv + coords %) [[0 1] [1 0] [0 -1] [-1 0]])))

(defn neigh8 [grid coords]
  (filter grid (map #(mapv + coords %) [[0 1] [1 0] [0 -1] [-1 0]
                                        [1 -1] [1 1] [-1 -1] [-1 1]])))

;;this is a tricolor graph-traversal algo
;;  breadth-first because using FIFO queue
;;  white = undiscovered: not marked, not in queue
;;  gray = frontier: marked but still in queue
;;  black = done; marked and no longer in queue
(defn breadth-first [init-set node-map neigh-fn max-dist]
  ;;start with intial (root) nodes marked and in the queue (starting frontier)
  (loop [todo (into #queue[] init-set)
         ;;inital nodes are marked with a value of 0
         marked (transient (zipmap init-set (repeat 0)))
         unmarked (transient (apply disj (set (keys node-map)) init-set))
         dist 0]
    (if (or (> dist max-dist) (empty? todo))
      (persistent! marked) ;;done
      ;; the queue is immutable, so peek and pop are really like first and rest
      (let [curr (peek todo) ;; the first node in the todo queue
            ;; all its unmarked (undiscovered) neighbors:
            unmar-neigh (neigh-fn unmarked curr)
            ;; they get value + 1
            new-val (inc (get marked curr))]
        ;;put all unmarked neighbors in todo and mark them (new frontier)
        ;;(the first node in todo -- already marked -- is removed)
        (recur (into (pop todo) unmar-neigh) 
               (reduce #(assoc! %1 %2 new-val) marked unmar-neigh)
               (reduce disj! unmarked unmar-neigh)
               new-val
               )))))

#_(let [g (zipmap (for [x (range 100) y (range 100)] [x y]) (repeat 1))]
    (time (let  
            [f (breadth-first
                #{[0 0] [99 99]}
                g
                neigh4
                30)]
            nil
            )))

