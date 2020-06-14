
(ns app.drawcanvas
  (:require [clojure.core.async :as a :refer [put! chan]]))

;; just functions for drawing to the display

(def GRAB-DIMS [16 16])
(def TILE-DIMS [24 24])

;; mutable drawing context
;;   may be read/written concurrently by other processes
(defonce context (atom {:ctx nil
                        :dims [0 0]
                        :tiles nil
                        :tilemap {}
                        :icons {}
                        ;;store drawing boundaries when map grid is larger than screen
                        :breakpoints {:x [] :y []}
                        }))

(def defaults {:tilemap {"@" (mapv * GRAB-DIMS [25 0])
                         "." (mapv * GRAB-DIMS [11 0])
                         "," (mapv * GRAB-DIMS [0 2])
                         " " (mapv * GRAB-DIMS [5 0])
                         "#" (mapv * GRAB-DIMS [13 16])
                         "`" (mapv * GRAB-DIMS [19 0])
                         "o" (mapv * GRAB-DIMS [10 6])
                         "u" (mapv * GRAB-DIMS [11 6])
                         "a" (mapv * GRAB-DIMS [32 10])
                         "P" (mapv * GRAB-DIMS [28 4])}
                 :icons {:player "@"
                         :floor "."
                         :grass ","
                         :empty " "
                         :wall "#"
                         :wall-s "`"
                         :closed-box "o"
                         :open-box "u"
                         :ananas "a"
                         :pedro "P"}})

(defn reset-defaults []
  (swap! context merge defaults))

(defn set-breakpts! [m]
  (swap! context assoc :breakpoints m))

;; calculates drawing boundaries when map grid is larger than screen
(defn calc-breakpoints-shift [[x y] [a b]]
  {:x (map #(+ (quot (rem x a) 2) %) (map #(* % a) (range (inc (quot x a)))))
   :y (map #(+ (quot (rem y b) 2) %) (map #(* % b) (range (inc (quot y b)))))})

;; no breakpoints, just center view
(defn calc-breakpoints-center [_ [a b]]
  {:x (dec (quot a 2)) :y (dec (quot b 2))})

(def calc-breakpoints calc-breakpoints-shift)

;;returns max breakpoints less than focal coords
(defn get-grid-start-shift [breaks [i j]]
  (let [xs (filter #(<= % i) (:x breaks))
        ys (filter #(<= % j) (:y breaks))]
    [(or (apply max xs) 0) (or (apply max ys) 0)]))

;; just centers the view
(defn get-grid-start-center [breaks [i j]]
  [(- i (:x breaks)) (- j (:y breaks))])

(def get-grid-start get-grid-start-shift)

;;convert grid coords to screen coords
(defn screen-coords [grid-coords zero-coords]
  (mapv * TILE-DIMS (mapv - grid-coords zero-coords)))

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
(defn draw-glow [d-context mkvs]
  (let [{:keys [ctx grid-start]} d-context]
    (doseq [[k v] mkvs]
      (draw-rect ctx (screen-coords k grid-start) TILE-DIMS [255 255 0 (* v 0.2)])
      )))

;;draws shadows based on k-v data
;; keys are coords; vals are light intensity
(defn draw-shadow [d-context mkvs]
  (let [{:keys [ctx grid-start]} d-context]
    (doseq [[k v] mkvs]
      (draw-rect ctx (screen-coords k grid-start) TILE-DIMS [0 0 0 (* 0.5 (- 1.0 v))])
      )))

;;draws a steady shadow over the coords in the given vector
(defn draw-dark [d-context vec]
  (let [{:keys [ctx grid-start]} d-context]
    (doseq [k vec]
      (draw-rect ctx (screen-coords k grid-start) TILE-DIMS [0 0 0 0.6])
      )))

;;draws a named object to xy coords contained in k
(defn- draw-coords [d-context clear? [k v]]
  (let [{:keys [ctx tiles tilemap icons grid-start]} d-context
        xy (screen-coords k grid-start)
        sxy (get tilemap (get icons v))]
    (when clear? (clear-tile ctx xy TILE-DIMS))
    (draw-tile ctx tiles sxy GRAB-DIMS xy TILE-DIMS)
    ))

;;draws a map of key-value pairs where each key is [x y] coords
;;  each value is a keyword
;;  doseq iterates through k-v pairs
(defn- draw-mkvs [d-context clear? mkvs]
  (doseq [m mkvs]
    (draw-coords d-context clear? m)))

;; takes entity val
(defn- draw-entity [d-context {:keys [id coords fov]}]
  (let [{:keys [ctx tiles tilemap icons grid-start]} d-context
        xy (screen-coords coords grid-start)
        sxy (get tilemap (get icons id))]
    (draw-tile ctx tiles sxy GRAB-DIMS xy TILE-DIMS)
    ;;TODO: make a separate function that decides when to light up field-of-view
    (when-not (= id :player) (draw-glow d-context fov))
    ))

(defn- clear-canvas [d-context]
  (let [{:keys [ctx dims]} d-context
        [w h] dims]
    (.clearRect ctx 0 0 w h)))

;; draws grid and entities
;;   in the "visible" area of the world grid only
(defn draw-visible [context-map world-state focal-coords]
  ;;update drawing area based on pre-calculated breakpoints
  (let [d-context (assoc context-map :grid-start 
                         (get-grid-start (:breakpoints context-map) focal-coords))
        seen-set (:seen world-state)
        vismap (:visible world-state)
        visible-entities (filter #(contains? vismap (:coords %))
                                 (vals (:entities world-state)))]
    
    ;;draw previously seen areas first
    (draw-mkvs d-context true (select-keys (:grid world-state) seen-set))
    (draw-mkvs d-context false (select-keys (:decor world-state) seen-set))
    (draw-dark d-context seen-set)

    ;;then draw over those with currently visible area
    (draw-mkvs d-context true (select-keys (:grid world-state) (keys vismap)))
    (draw-mkvs d-context false (select-keys (:decor world-state) (keys vismap)))
    (draw-shadow d-context vismap)

    ;;draw entities whose coords are in the visible area
    (doseq [e visible-entities]
      (draw-entity d-context e)
      )))

;; draw-element multimethod
;;   draws different things depending on :type key
(defmulti draw-element (fn [elem focused? d-context] (:type elem)))

;; :type :grid draws the world grid 
;;   (element's :data key is a function that returns it)
(defmethod draw-element :grid [{:keys [data]} _ d-context]
  (let [{:keys [world-state focal-coords]} (data)]
    (draw-visible d-context world-state focal-coords)
    #_(draw-visible d-context 
                  (assoc world-state 
                         :seen #{}
                         :visible (zipmap 
                                   (keys (merge (:decor world-state) (:grid world-state)))
                                   (repeat 1)))
                  focal-coords)
    ))

;; :type :button draws a text button
;;   element's :data key contains a function
(defmethod draw-element :button [{:keys [txt pos]} focused? {:keys [ctx]}]
  (set! (. ctx -font) "24px monospace")
  (draw-text ctx txt
             (mapv * TILE-DIMS (mapv + [20 10] [0 (* 1.5 pos)]))
             (if focused? [255 255 80] [180 180 180])))

;; :type :checkbox
;;   element's :data key contains a function
(defmethod draw-element :checkbox 
  [{:keys [txt pos data] :or {data (fn [_] nil)}} focused? {:keys [ctx]}]
  (set! (. ctx -font) "24px monospace")
  (draw-text ctx (str (if (data) "[*] " "[ ] ") txt)
             (mapv * TILE-DIMS (mapv + [20 10] [0 (* 1.5 pos)]))
             (if focused? [255 255 80] [180 180 180])))

(defmethod draw-element :panel [{:keys [pos]} _ d-context]
  ;;draw a box
  )

;; :type :cursor
;; :data fn returns coords
(defmethod draw-element :cursor [{:keys [data]} _ {:keys [ctx breakpoints]}]
  (let [coords (data)
        grid-start (get-grid-start breakpoints coords)]
    (set! (. ctx -font) "24px monospace")
    (draw-rect ctx (screen-coords coords grid-start) TILE-DIMS [255 255 0 0.4])
    ))

;; :type :target-info
;; :data fn is world/get-info
(defmethod draw-element :target-info [{:keys [pos data]} _ {:keys [ctx]}]
  (let [;; get target coords and world state
        {:keys [coords grid seen? visible? entities]} (data)
        ;; TODO - figure out where text descriptions should live
        txt (str coords " " (when seen? grid) " " (when visible? entities))
        ]
    (set! (. ctx -font) "16px monospace")
    (draw-text ctx txt (mapv * TILE-DIMS pos) [255 255 255])
    ))

;; :type :time-log
(defmethod draw-element :time-log [{:keys [pos data] :or {data (fn [_] nil)}} _ {:keys [ctx]}]
  (let [logv (data)
        data-vec (subvec logv (max 0 (- (count logv) 5)))
        current-time (:time (peek data-vec))]
    (set! (. ctx -font) "16px monospace")
    (loop [[{:keys [msg time]} & r] data-vec
           p pos]
      (let [rel-time (- time current-time)
            b (* 255 (Math/pow 1.03 rel-time))]
        (draw-text ctx msg (mapv * TILE-DIMS p) [b b b]))
      (when (seq r) (recur r (mapv + [0 1] p)))
      )))

;; called by render-ui, takes collection of elements to draw
(defn draw-group [coll focused-elem d-context]
  (doseq [x coll]
    (draw-element x (= x focused-elem) d-context)
    (when-let [sub-coll (:elements x)]
      (draw-group sub-coll focused-elem d-context))
    ))

;; called from main, takes UI db
(defn render-ui [{:keys [ui-components background focused foreground]}]
  (let [[fg-key _] focused
        d-context @context]
    (clear-canvas d-context)
    ;;do background comps first
    ;;  iterate through items within the component (no item is focused here)
    (doseq [k background] (draw-group (get ui-components k) nil d-context))
    ;;render the focused comp on top; focused sub-element may be drawn differently
    (draw-group (get ui-components fg-key) (get-in ui-components focused) d-context)
    ;;overlay foreground comps
    (doseq [k foreground] (draw-group (get ui-components k) nil d-context))
    ))

