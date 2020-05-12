
(ns app.main
  (:require ["rot-js" :as rot]
            [clojure.core.async :as a :refer [>! <! put! go go-loop chan]]
            [app.world :as world :refer [world-state init-grid! move-player! open-box!]]
            [app.drawrot :as draw :refer [init-disp! redraw-entity re-draw]]))

(defonce key-chan (chan)) ;;channel for processing keyboard input

;;initialize world-map and interface with given width and height
(defn init! [w h]
  (init-grid! w h)
  (init-disp! w h)
  (re-draw @world-state))

;;maps input codes to functions
(def keymap {[37] #(move-player! 6)
             [:shift 37] #(move-player! 7)
             [38] #(move-player! 0)
             [:shift 38] #(move-player! 1)
             [39] #(move-player! 2)
             [:shift 39] #(move-player! 3)
             [40] #(move-player! 4)
             [:shift 40] #(move-player! 5)
             [13] #(open-box!)
             [32] #(open-box!)})

;;called on player's turn to process input
(defn handle-input [code]
  (when-let [f (get keymap code)]
    (f)))

;; from the point of view of the world model, player and npcs are just game entities
;; but the user interface needs to handle them differently
;; use multi-method dispatch for this, based on a ":type" key
;;   methods for specific values of :type are defined below
(defmulti take-turn #(:type %))


;; this method is called on the player's turn & waits for keyboard input
;; go-loop spawns a process that will eventually put something on out channel
;; (<! key-chan) will park until another fn puts something on key-chan
;; immediately returns out channel
(defmethod take-turn :local-player [_]
  (let [out (chan)]
    (go-loop []
      ;;wait for a code from key-chan
      (let [code (<! key-chan)
            ;;fn's called by handle-input return a result if player made a move
            result (handle-input code)]
        (if result
          ;;if there's a result, send it to out channel and exit loop
          (>! out result)
          ;;else, continue the go-loop
          (recur))))
    out))

;; this method is called on an npc's turn & calls a fn defined in the world state
;; an entity is a hashmap
;;   contains :id key for looking itself up in world-state
(defmethod take-turn :npc [entity]
  (let [out (chan)]
    (go
      (let [entity-fn (:action entity)
            result (entity-fn)]
        (>! out result)))
    out))

;; put game rule logic in this fn for now
(defn game-rules [result]
  (cond
    (:ananas result) (do
                       (js/alert "The box contains a golden pineapple, heavy with promise and juice.\n\nIt shines as you hold it aloft. In the distance, Pedro howls in rage.")
                       (swap! draw/icons #(assoc % :player (get % :ananas))))
    (= 0 (:path-length result)) (do
                                  (js/alert "Pedro has caught you!\n\nYOU ARE THE NEW PEDRO!")
                                  (swap! draw/icons #(assoc % :player (get % :pedro)))
                                  (swap! world-state assoc-in [:entities :pedro :action] (fn [_] {:pedro :skip})))))

;; turn-loop spawns a looping process that allocates game turns
(defn turn-loop [scheduler ch]
  ;; get the next entity key from the scheduler
  (go-loop [entity-key (.next scheduler)]
          ;;save a snapshot of the world state
    (let [state @world-state
          ;;save a snapshot of the entity
          entity (get (:entities state) entity-key)
          ;; (take-turn) will change the world state, puts result on a channel
          ;; <! (take-turn) will park until something is put on that channel
          result (if entity
                   (<! (take-turn entity))
                   {entity-key false})]
      ;; apply game rules based on the result or the world-state
      (game-rules result)
      ;;redraw changes made by current entity; (:coords entity) is the old position
      (redraw-entity @world-state entity-key (:coords entity))
      ;;pass result to output channel
      (>! ch result))
    (recur (.next scheduler))))

;; creates and populates a rot-js scheduler
;;   passes it to turn-loop
(defn start-turn-loop []
  (let [debug-ch (chan)
        scheduler (rot/Scheduler.Simple.)]
    ;;must add entity keys to the scheduler, not the entities themselves
    ;;-- clj data structures are immutable; entities added would just be snapshots
    (doseq [k (keys (:entities @world-state))]
      (.add scheduler k true))
    (turn-loop scheduler debug-ch)
    ;;start a process to montior the output channel
    (go (while true (println (<! debug-ch))))))

;; translates javascript keyboard event to input code
;;   the cond-> macro threads its first argument through pairs of statements
;;   applying the 2nd statement if the 1st is true
;;   cond-> [] with conj statements is a way to build up a vector
(defn key-event [e]
  (put! key-chan
        (cond-> []
          (. e -shiftKey) (conj :shift)
          (. e -ctrlKey) (conj :ctrl)
          :always (conj (. e -keyCode)))))



;; called when app first loads (as specified in shadow-cljs.edn)
(defn main! []
  (init! 60 40)
  (. js/document addEventListener "keydown" key-event)
  (start-turn-loop)
  )

;; hot reload; called by shadow-cljs when code is saved
;;   object state is preserved
(defn reload! []
  (re-draw @world-state)
  )





