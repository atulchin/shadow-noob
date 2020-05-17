
(ns app.drawcanvas
  (:require [clojure.core.async :as a :refer [>! <! put! go go-loop chan]]))

;; just functions for drawing to the display

(def GRAB-DIMS [16 16])
(def TILE-DIMS [24 24])

;; mutable drawing context
;;   may be read/written concurrently by other processes
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
                        :icons {:player "@"
                                :empty-grid "."
                                :closed-box "o"
                                :open-box "u"
                                :ananas "a"
                                :pedro "P"}
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

(defn- draw-tile [ctx tiles [sx sy] [grab-w grab-h] [x y] [tile-w tile-h]]
  (.drawImage ctx tiles sx sy grab-w grab-h x y tile-w tile-h)
  )

(defn- clear-tile [ctx [x y] [tile-w tile-h]]
  (.clearRect ctx x y tile-w tile-h)
  )

(defn- color-str [coll]
  (str "rgba(" (apply str (interpose "," coll)) ")"))

(defn- draw-rect [ctx [x y] [w h] color]
  (set! (. ctx -fillStyle) (color-str color))
  (.fillRect ctx x y w h))

(defn draw-glow [mkvs]
  (let [{:keys [ctx]} @context]
    (doseq [[k v] mkvs]
      (draw-rect ctx (mapv * TILE-DIMS k) TILE-DIMS [255 255 0 (* v 0.2)]))))

;;draws a named object to xy coords contained in k
(defn- draw-kv [d-context [k v]]
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
(defn- draw-grid [d-context mkvs]
  (doseq [m mkvs]
    (draw-kv d-context m)))

;; an entity is a key-val pair
;;  where the val is a hashmap containing a :coords key
(defn- draw-entity [d-context [k {:keys [coords fov]}]]
  (let [{:keys [ctx tiles tilemap icons]} d-context
        xy (mapv * coords TILE-DIMS)
        sxy (get tilemap (get icons k))]
    (draw-tile ctx tiles sxy GRAB-DIMS xy TILE-DIMS)
    (draw-glow fov)
    ))

(defn- clear-canvas [d-context]
  (let [{:keys [ctx dims]} d-context
        [w h] dims]
    (.clearRect ctx 0 0 w h))
  )

;; draws grid and entities from scratch
(defn re-draw [world-state]
  (let [d-context @context]
    (clear-canvas d-context)
    (draw-grid d-context (:grid world-state))
    (doseq [e (:entities world-state)] (draw-entity d-context e))
    ))

;; used after an entity moves
;;  draws entity at current loc & redraws map at old loc
(defn redraw-entity [world-state entity-key old-coords]
  (let [d-context @context]
    (draw-kv d-context (find (:grid world-state) old-coords))
    (draw-entity d-context (find (:entities world-state) entity-key))
    ))

