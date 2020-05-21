
(ns app.main
  (:require ["rot-js" :as rot]
            [clojure.core.async :as a :refer [>! <! put! go go-loop chan]]
            [app.world :as world :refer [world-state move-player! open-box!]]
            [app.drawcanvas :as draw :refer [render-ui]]))

;; this file is for user inteface & interaction w/ game state

;; TODO: game logic also lives here for now, may move it
(declare game-rules! game-options!)

;;channels for processing keyboard input
;;  key-chan is read by the main game screen
(defonce key-chan (chan))
;;  dialog-chan is read by ui dialogs/menus
(defonce dialog-chan (chan))
;; a channel to ensure that rendering does not happen until the 
;;   effects of ui inputs are processed (e.g., loading graphics)
(defonce control-ch (chan))

;; ui components, named by keyword
;;   effect fns are defined below
(declare set-option! new-game! about! char-menu! db)
(def ui-components {:start-menu [{:id :new-game :pos 0 :type :button :txt "New game" :effect #(char-menu!)}
                                 {:id :about :pos 1 :type :button :txt "About" :effect #(about!)}]
                    :char-menu [{:id :label :pos 0 :type :button :txt "Choose One"}
                                {:id :opt1 :pos 1 :type :checkbox :txt "Speed" :data #(get-in @db [:options :speed]) :effect #(set-option! :speed true :vision false)}
                                {:id :opt2 :pos 2 :type :checkbox :txt "Vision" :data #(get-in @db [:options :vision]) :effect #(set-option! :speed false :vision true)}
                                {:id :ok :pos 3 :type :button :txt "[  OK  ]" :effect #(new-game!)}]
                    ;; game-screen component contains a reference to world-state atom
                    :game-screen [{:id :world-map :type :grid :data #(deref world/world-state)}]})

;; all info needed for generating the interface
(defonce db (atom {:ui-components ui-components
                   :keychan dialog-chan
                   :focused [:start-menu 0]
                   :background []
                   :dims [60 40]
                   :options {}}))

(defn set-option! [& opts]
  (swap! db update :options #(apply assoc % opts))
  (put! control-ch true))

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
  ;;re-render ui (including game map) at start of player's turn
  (render-ui @db)
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

;; turn-loop spawns a looping process that allocates game turns
(defn turn-loop [scheduler ch]
  ;; get the next entity key from the scheduler
  (go-loop []
          ;;save a snapshot of the world state
    (let [entity-key (.next scheduler)
          state @world-state
          ;;save a snapshot of the entity
          entity (get-in state [:entities entity-key])
          ;; (take-turn) will change the world state, puts result on a channel
          ;; <! (take-turn) will park until something is put on that channel
          result (if entity
                   (<! (take-turn entity))
                   {entity-key false})]
      ;; apply game rules based on the result or the world-state
      (game-rules! result)

      ;; if result includes :time, set action duration in scheduler
      ;;   otherwise default duration
      (.setDuration scheduler (or (:time result) 10))

      ;; instead of redrawing changes caused by the entity's turn 
      ;;    here (could be lots), just redraw everything at start
      ;;    of player's turn
      
      ;;pass result to output channel
      (>! ch result))
    (recur)))

;; creates and populates a rot-js scheduler
;;   passes it to turn-loop
(defn start-turn-loop []
  (let [debug-ch (chan)
        scheduler (rot/Scheduler.Action.)
        state @world-state]
    ;;must add entity keys to the scheduler, not the entities themselves
    ;;-- clj data structures are immutable; entities added would just be snapshots
    (doseq [k (keys (:entities state))]
      (.add scheduler k true))
    (turn-loop scheduler debug-ch)
    ;;start a process to montior the output channel
    (go (while true (println (<! debug-ch))))))

;; translates javascript keyboard event to input code
;; puts it on the channel specified in UI database
;;   the cond-> macro threads its first argument through pairs of statements
;;   applying the 2nd statement if the 1st is true
;;   cond-> [] with conj statements is a way to build up a vector
(defn key-event [e]
  (put! (:keychan @db)
        (cond-> []
          (. e -shiftKey) (conj :shift)
          (. e -ctrlKey) (conj :ctrl)
          :always (conj (. e -keyCode)))))

;; functions called by ui components
(defn new-game! []
  (swap! db assoc :keychan key-chan :focused [:game-screen 0] :background [])
  ;;init will send the ok to the control channel
  (world/init-grid! (:dims @db))
  (game-options! (:options @db))
  (start-turn-loop)
  ;; ok to proceed
  (put! control-ch true))

(defn about! []
  (println "about")
  ;; ok to proceed
  (put! control-ch true))

(defn char-menu! []
  (swap! db assoc :focused [:char-menu 0] :background [])
  (put! control-ch true))

;; ui controls
(defn move-focus! [i]
  (let [{ui-comps :ui-components [comp-key idx] :focused} @db
        n (count (get ui-comps comp-key))
        x (+ i idx)]
    (swap! db assoc :focused
           (cond
             (< x 0) [comp-key (+ x n)]
             (>= x n) [comp-key (- x n)]
             :else [comp-key x]))
    ;; ok to proceed
    (put! control-ch true)))

(defn click! []
  (let [{ui-comps :ui-components key-vec :focused} @db
        comp (get-in ui-comps key-vec)]
    (if-let [f (:effect comp)]
      ;; the effect must send a result to control channel
      (f)
      ;; else, comp has no effect, ok to proceed
      (put! control-ch true))))

;;maps input codes to functions for dialogs/menus
(def dialog-keymap {[37] #(move-focus! -1)
                    [38] #(move-focus! -1)
                    [39] #(move-focus! 1)
                    [40] #(move-focus! 1)
                    [13] #(click!)
                    [32] #(click!)})

;;called by dialog-loop
(defn dialog-input [code]
  (when-let [f (get dialog-keymap code)]
    (f)))

;;spawns a process that listens for keyboard codes sent to dialog-chan
(defn dialog-loop []
  (go-loop []
    (let [code (<! dialog-chan)]
      (dialog-input code)
      ;;render new ui state after every dialog input
      (when (<! control-ch) (render-ui @db))
      (recur))))

;; called when app first loads (as specified in shadow-cljs.edn)
(defn main! []
  (. js/document addEventListener "keydown" key-event)
  (dialog-loop)
  (go
    (when (<! (draw/init-disp! (:dims @db)))
      (render-ui @db))))

;; hot reload; called by shadow-cljs when code is saved
;;   object state is preserved
(defn reload! []
  (render-ui @db))



;; put game rule logic in this fn for now
(defn game-rules! [result]
  (cond
    (:ananas result) (do
                       (js/alert "The box contains a golden pineapple, heavy with promise and juice.\n\nIt shines as you hold it aloft. In the distance, Pedro howls in rage.")
                       (swap! draw/context #(assoc-in % [:icons :player] (get-in % [:icons :ananas]))))
    (= 0 (:path-length result)) (do
                                  (js/alert "Pedro has caught you!\n\nYOU ARE THE NEW PEDRO!")
                                  (swap! draw/context #(assoc-in % [:icons :player] (get-in % [:icons :pedro])))
                                  (swap! world-state assoc-in [:entities :pedro :action] (fn [_] {:pedro :skip})))))


;; put game option logic in this fn for now
(defn game-options! [{:keys [speed vision]}]
  (swap! world-state update-in [:entities :player]
         #(assoc %
                 :fov-fn (if vision :fov-360 :fov-90)
                 :diag-time (if speed 10 14)
                 )))
