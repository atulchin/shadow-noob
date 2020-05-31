
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

;; true if the value of key k in map m is in the keys of s
(defn contains-val? [s k m]
  (contains? s (get m k)))
