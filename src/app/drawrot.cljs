
(ns app.drawrot
  (:require ["rot-js" :as rot]))

;; just functions for drawing to the display

(def disp-options #js{
                      ;:width 60 :height 40
                      :fontFamily "DejaVu Sans Mono"
                      :forceSquareRatio true
                      :fg "#aaa"
                      })

;; display object from rot-js
;; "defonce" avoids contructing a new one when code is hot-reloaded
(defonce disp (rot/Display. disp-options))

;; mutable icons and colors for named keywords
;;   may be read/written concurrently by other processes
(defonce icons (atom {:player "@"
                      :empty-grid "⬩"
                      :closed-box "☒"
                      :open-box "☐"
                      :ananas "🍍"
                      :pedro "P"}))
(defonce colors (atom {:player "#ff0"
                       :empty-grid "#bbb"
                       :closed-box "#4f6"
                       :open-box "#4f6"
                       :ananas "#ff0"
                       :pedro "red"}))

;;set display options and add it to the document body
(defn init-disp! [w h]
  ;;set javascript properties using (set!)
  (set! (.-id (.getContainer disp)) "canvas-id")
  ;;(.getContainer) returns a <canvas> element; append it to the html body
  (.appendChild (. js/document -body) (.getContainer disp))
  (.setOptions disp #js{
                        :width w
                        :height h
                        ;:fontSize (.computeFontSize disp (. js/window -innerWidth) (. js/window -innerHeight))
                        :fontSize 23
                        }))

;;draws a named object to xy coords contained in k
(defn draw-kv [[k v]]
  (.draw disp (first k) (second k) (get @icons v) (get @colors v)))

;;draws a map of key-value pairs where each key is [x y] coords
;;  each value is a keyword
;;  doseq iterates through k-v pairs
(defn draw-grid [mkvs]
  (doseq [m mkvs]
    (draw-kv m)))

;; an entity is a key-val pair
;;  where the val is a hashmap containing a :coords key
(defn draw-entity [[k {:keys [coords]}]]
  (.draw disp (first coords) (second coords) (get @icons k) (get @colors k)))

;; draws grid and entities from scratch
(defn re-draw [world-state]
  (.clear disp)
  (draw-grid (:grid world-state))
  (doseq [e (:entities world-state)] (draw-entity e))
  )

;; used after an entity moves
;;  draws entity at current loc & redraws map at old loc
(defn redraw-entity [world-state entity-key old-coords]
  (draw-kv (find (:grid world-state) old-coords))
  (draw-entity (find (:entities world-state) entity-key)))
