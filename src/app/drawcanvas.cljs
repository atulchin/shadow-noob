
(ns app.drawcanvas
  (:require [clojure.core.async :as a :refer [>! <! put! go go-loop chan]]))

;; just functions for drawing to the display

(def GRAB-DIMS [16 16])
(def TILE-DIMS [24 24])

;; mutable icons and colors for named keywords
;;   may be read/written concurrently by other processes
(defonce icons (atom {:player "@"
                      :empty-grid "."
                      :closed-box "o"
                      :open-box "u"
                      :ananas "a"
                      ;:closed-box "☒"
                      ;:open-box "☐"
                      ;:ananas "🍍"
                      :pedro "P"}))
(defonce colors (atom {:player "#ff0"
                       :empty-grid "#bbb"
                       :closed-box "#4f6"
                       :open-box "#4f6"
                       :ananas "#ff0"
                       :pedro "red"}))


(defonce context (atom {
                        :ctx nil
                        :dims [0 0]
                        :tiles nil
                        :tilemap {"@" (mapv * GRAB-DIMS [25 0])
                                  "." (mapv * GRAB-DIMS [11 0])
                                  "o" (mapv * GRAB-DIMS [10 6])
                                  "u" (mapv * GRAB-DIMS [11 6])
                                  "a" (mapv * GRAB-DIMS [32 10])
                                  "P" (mapv * GRAB-DIMS [28 4])}
                        }))

 
;;set display options and add it to the document body
(defn init-disp! [w h]
  ;;this fn can return before tiles are loaded
  ;;  return a channel that lets us know when tiles are ready to draw
  (let [out (chan)
        canvas (.createElement js/document "canvas")
        ctx (.getContext canvas "2d")
        tiles (.createElement js/document "img")
        dims (mapv * TILE-DIMS [w h])]

    (set! (. tiles -onload) #(put! out true))
    (set! (. tiles -src) "bitpack.png")
    (set! (. canvas -width) (first dims))
    (set! (. canvas -height) (second dims))
    (set! (. ctx -imageSmoothingEnabled) false)
    (set! (. ctx -mozImageSmoothingEnabled) false)
    (set! (. ctx -webkitImageSmoothingEnabled) false)

    ;;append canvas to the html body
    (.appendChild (. js/document -body) canvas)
    (swap! context merge {:ctx ctx
                          :dims dims
                          :tiles tiles})
    out))

(defn draw-tile [ctx tiles [sx sy] [grab-w grab-h] [x y] [tile-w tile-h]]
  (.drawImage ctx tiles sx sy grab-w grab-h x y tile-w tile-h)
  )

(defn clear-tile [ctx [x y] [tile-w tile-h]]
  (.clearRect ctx x y tile-w tile-h)
  )

;;draws a named object to xy coords contained in k
(defn draw-kv [[k v]]
  (let [{:keys [ctx tiles tilemap]} @context
        xy (mapv * k TILE-DIMS)
        sxy (get tilemap (get @icons v))]
    ;; clear first
    (clear-tile ctx xy TILE-DIMS)
    (draw-tile ctx tiles sxy GRAB-DIMS xy TILE-DIMS)
    ))

;;draws a map of key-value pairs where each key is [x y] coords
;;  each value is a keyword
;;  doseq iterates through k-v pairs
(defn draw-grid [mkvs]
  (doseq [m mkvs]
    (draw-kv m)))

;; an entity is a key-val pair
;;  where the val is a hashmap containing a :coords key
(defn draw-entity [[k {:keys [coords]}]]
  (let [{:keys [ctx tiles tilemap]} @context
        xy (mapv * coords TILE-DIMS)
        sxy (get tilemap (get @icons k))]
    (draw-tile ctx tiles sxy GRAB-DIMS xy TILE-DIMS)
    ))

(defn clear-canvas []
  (let [{:keys [ctx dims]} @context
        [w h] dims]
    (.clearRect ctx 0 0 w h))
  )

;; draws grid and entities from scratch
(defn re-draw [world-state]
  (clear-canvas)
  (draw-grid (:grid world-state))
  (doseq [e (:entities world-state)] (draw-entity e))
  )

;; used after an entity moves
;;  draws entity at current loc & redraws map at old loc
(defn redraw-entity [world-state entity-key old-coords]
  (draw-kv (find (:grid world-state) old-coords))
  (draw-entity (find (:entities world-state) entity-key)))
