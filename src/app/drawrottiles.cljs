
(ns app.drawrottiles
  (:require ["rot-js" :as rot]
            [clojure.core.async :as a :refer [>! <! put! go go-loop chan]]))

;; just functions for drawing to the display

(def TILEW 16)
(def TILEH 16)

(def tilemap (clj->js {
                       "@" [(* TILEW 25) 0]
                       "." [(* TILEW 11) (* TILEH 0)]
                       "o" [(* TILEW 10) (* TILEH 6)]
                       "u" [(* TILEW 11) (* TILEH 6)]
                       "a" [(* TILEW 32) (* TILEH 10)]
                       "P" [(* TILEW 28) (* TILEH 4)]
                       }))

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

(def disp-options
  #js{:layout "tile"
      :bg "transparent"
      ;:tileColorize true
      :tileWidth TILEW
      :tileHeight TILEH
      :tileMap tilemap
      })

;; display object from rot-js
;; "defonce" avoids contructing a new one when code is hot-reloaded
(defonce disp (rot/Display. disp-options))

(defonce tiles (.createElement js/document "img"))

;;set display options and add it to the document body
(defn init-disp! [w h]
  ;;this fn can return before tiles are loaded
  ;;  return a channel that lets us know when tiles are ready to draw
  (let [out (chan)]
    ;;set javascript properties using (set!)
    (set! (.-id (.getContainer disp)) "canvas-id")

    ;;(.getContainer) returns a <canvas> element; append it to the html body
    (.appendChild (. js/document -body) (.getContainer disp))

    (set! (. tiles -onload) #(put! out true))
    (set! (. tiles -src) "bitpack.png")

    (.setOptions disp #js{:tileSet tiles
                          :width w
                          :height h
                        ;:fontSize (.computeFontSize disp (. js/window -innerWidth) (. js/window -innerHeight))
                        ;:fontSize 23
                        ;:fontFamily "DejaVu Sans Mono"
                        ;:forceSquareRatio true
                        ;:fg "#aaa"
                          })
    out))

;;draws a named object to xy coords contained in k
(defn draw-kv [[k v]]
  ;(.draw disp (first k) (second k) (get @icons v) (get @colors v))
  (.draw disp (first k) (second k) (get @icons v))
  )

;;draws a map of key-value pairs where each key is [x y] coords
;;  each value is a keyword
;;  doseq iterates through k-v pairs
(defn draw-grid [mkvs]
  (doseq [m mkvs]
    (draw-kv m)))

;; an entity is a key-val pair
;;  where the val is a hashmap containing a :coords key
(defn draw-entity [[k {:keys [coords]}]]
  ;(.draw disp (first coords) (second coords) (get @icons k) (get @colors k))
  (.draw disp (first coords) (second coords) (get @icons k))
  )

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
