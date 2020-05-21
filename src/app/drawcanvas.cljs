
(ns app.drawcanvas
  (:require [clojure.core.async :as a :refer [>! <! put! go go-loop chan]]))

;; just functions for drawing to the display

(def GRAB-DIMS [16 16])
(def TILE-DIMS [24 24])

;; mutable drawing context
;;   may be read/written concurrently by other processes
(defonce context (atom {:ctx nil
                        :dims [0 0]
                        :tiles nil
                        :tilemap {}
                        :icons {}}))

(def defaults {:tilemap {"@" (mapv * GRAB-DIMS [25 0])
                           "." (mapv * GRAB-DIMS [11 0])
                           "o" (mapv * GRAB-DIMS [10 6])
                           "u" (mapv * GRAB-DIMS [11 6])
                           "a" (mapv * GRAB-DIMS [32 10])
                           "P" (mapv * GRAB-DIMS [28 4])}
                 :icons {:player "@"
                         :empty-grid "."
                         :closed-box "o"
                         :open-box "u"
                         :ananas "a"
                         :pedro "P"}})

(defn reset-defaults []
  (swap! context merge defaults))

;;set display options and add it to the document body
(defn init-disp! [[w h]]
  ;;this fn can return before tiles are loaded
  ;;  return a channel that lets us know when tiles are ready to draw
  (let [out (chan)
        canvas (.createElement js/document "canvas")
        ctx (.getContext canvas "2d")
        tiles (.createElement js/document "img")
        dims (mapv * TILE-DIMS [w h])]

    ;; tiles .onload event sends signal to out channel
    (set! (. tiles -onload) #(put! out true))
    (set! (. tiles -src) "bitpack.png")
    (set! (. canvas -width) (first dims))
    (set! (. canvas -height) (second dims))
    (set! (. ctx -imageSmoothingEnabled) false)
    (set! (. ctx -mozImageSmoothingEnabled) false)
    (set! (. ctx -webkitImageSmoothingEnabled) false)
    (set! (. ctx -font) "24px monospace")

    ;;append canvas to the html body
    (.appendChild (. js/document -body) canvas)
    (swap! context merge {:ctx ctx
                          :dims dims
                          :tiles tiles})
    out))

(defn- draw-tile [ctx tiles [sx sy] [grab-w grab-h] [x y] [tile-w tile-h]]
  (.drawImage ctx tiles sx sy grab-w grab-h x y tile-w tile-h))

(defn- clear-tile [ctx [x y] [tile-w tile-h]]
  (.clearRect ctx x y tile-w tile-h))

;; converts vector to js color argument
(defn- color-str [coll]
  (str "rgba(" (apply str (interpose "," coll)) ")"))

(defn- draw-rect [ctx [x y] [w h] color]
  (set! (. ctx -fillStyle) (color-str color))
  (.fillRect ctx x y w h))

(defn- draw-text [ctx txt [x y] color]
  (set! (. ctx -fillStyle) (color-str color))
  (.fillText ctx txt x y))

;;draws a yellow glow based on k-v data
;;  keys are coords; vals are light intensity
(defn draw-glow [mkvs]
  (let [{:keys [ctx]} @context]
    (doseq [[k v] mkvs]
      (draw-rect ctx (mapv * TILE-DIMS k) TILE-DIMS [255 255 0 (* v 0.2)])
      )))

;;draws shadows based on k-v data
;; keys are coords; vals are light intensity
(defn draw-shadow [mkvs]
  (let [{:keys [ctx]} @context]
    (doseq [[k v] mkvs]
      (draw-rect ctx (mapv * TILE-DIMS k) TILE-DIMS [0 0 0 (* 0.5 (- 1.0 v))])
      )))

;;draws a steady shadow over the coords in the given vector
(defn draw-dark [vec]
  (let [{:keys [ctx]} @context]
    (doseq [k vec]
      (draw-rect ctx (mapv * TILE-DIMS k) TILE-DIMS [0 0 0 0.6])
      )))

;;draws a named object to xy coords contained in k
(defn- draw-coords [d-context [k v]]
  (let [{:keys [ctx tiles tilemap icons]} d-context
        xy (mapv * k TILE-DIMS)
        sxy (get tilemap (get icons v))]
    ;; clear first
    (clear-tile ctx xy TILE-DIMS)
    (draw-tile ctx tiles sxy GRAB-DIMS xy TILE-DIMS)
    ))

;;draws a map of key-value pairs where each key is [x y] coords
;;  each value is a keyword
;;  doseq iterates through k-v pairs
(defn- draw-mkvs [d-context mkvs]
  (doseq [m mkvs]
    (draw-coords d-context m)))

;;limits drawing to the coords present in visibility map
(defn- draw-vis-grid [d-context full-grid vismap]
  (let [mkvs (select-keys full-grid (keys vismap))]
    (draw-mkvs d-context mkvs)
    (draw-shadow vismap)))

;;draws previously visible coords (listed in seen-set) overlaid with shadow
(defn- draw-seen [d-context full-grid seen-set]
  (let [mkvs (select-keys full-grid seen-set)]
    (draw-mkvs d-context mkvs)
    (draw-dark seen-set)))

;; an entity is a key-val pair
;;  where the val is a hashmap containing a :coords key
(defn- draw-entity [d-context [k {:keys [coords fov]}]]
  (let [{:keys [ctx tiles tilemap icons]} d-context
        xy (mapv * coords TILE-DIMS)
        sxy (get tilemap (get icons k))]
    (draw-tile ctx tiles sxy GRAB-DIMS xy TILE-DIMS)
    ;;TODO: make a separate function that decides when to light up field-of-view
    (when-not (= k :player) (draw-glow fov))
    ))

(defn- clear-canvas [d-context]
  (let [{:keys [ctx dims]} d-context
        [w h] dims]
    (.clearRect ctx 0 0 w h)))

;; draws grid and entities
;;   in the "visible" area of the world grid only
(defn draw-visible [world-state]
  (let [d-context @context]
    ;;draw previously seen areas first
    (draw-seen d-context (:grid world-state) (:seen world-state))
    ;;then draw over those with currently visible area
    (draw-vis-grid d-context (:grid world-state) (:visible world-state))
    ;;draw entities whose coords are in the visible area
    (doseq [e (:entities world-state)]
      (when (get (:visible world-state) (:coords (val e)))
        (draw-entity d-context e))
      )))

;;   the entire world grid & all entities
(defn draw-full [world-state]
  (let [d-context @context]
    (draw-mkvs d-context (:grid world-state))
    (doseq [e (:entities world-state)] (draw-entity d-context e))
    ))

;; draw-element multimethod
;;   draws different things depending on :type key
(defmulti draw-element #(:type %))

;; :type :grid draws the world grid 
;;   (element's :data key is a function that returns it)
(defmethod draw-element :grid [{:keys [data]}]
  (draw-visible (data)))

;; :type :button draws a text button
;;   element's :data key contains a function
(defmethod draw-element :button [{:keys [txt pos focused?]}]
  (draw-text (:ctx @context)
             txt
             (mapv * TILE-DIMS (mapv + [20 10] [0 (* 1.5 pos)]))
             (if focused? [255 255 80] [180 180 180])))

;; :type :checkbox
;;   element's :data key contains a function
(defmethod draw-element :checkbox [{:keys [txt pos focused? data] :or {data (fn [_] nil)}}]
  (draw-text (:ctx @context) 
             (str (if (data) "[*] " "[ ] ") txt) 
             (mapv * TILE-DIMS (mapv + [20 10] [0 (* 1.5 pos)]))
             (if focused? [255 255 80] [180 180 180])
             ))

;; called by render-ui, takes collection of elements to draw
(defn draw-group [coll focused-elem]
  (doseq [x coll]
    (draw-element (assoc x :focused? (= x focused-elem)))
    ))

;; called from main, takes UI db
(defn render-ui [{:keys [ui-components background focused]}]
  (clear-canvas @context)
  (let [[fg-key _] focused]
    ;;do background comps first
    (doseq [k background]
      ;;iterate through items within the component (no item is focused here)
      (draw-group (get ui-components k) nil))
    ;;render the focused comp on top; focused sub-element may be drawn differently
    (draw-group (get ui-components fg-key) (get-in ui-components focused))
    ))

