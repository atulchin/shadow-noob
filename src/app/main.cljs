
(ns app.main
  (:require ["rot-js" :as rot]
            [clojure.core.async :as a :refer [>! <! put! go go-loop chan dropping-buffer]]
            [app.world :as world :refer [world-state move-player! open-box!]]
            [app.drawcanvas :as draw :refer [init-disp! render-ui]]))

;; this file is for manipulating the user inteface & interacting w/ game state
;; TODO: game logic also lives here for now, may move it
(declare game-rules! game-options!)

;;channels for processing input
;;  key-chan is read by the main game screen
(defonce key-chan (chan (dropping-buffer 5)))
;;  dialog-chan is read by ui dialogs/menus
(defonce dialog-chan (chan (dropping-buffer 5)))
;;  control-chan is read during player's turn
;;    other processes should send functions to it
(defonce control-chan (chan (dropping-buffer 5)))

;; ui components, named by keyword
;;  TODO later: figure out a clean way to send data so you can break ui
;;    out into a separate lib
(declare set-option! new-game! about! char-menu! restart! db)
;; a component is a vector of maps
;;    a compoment can contain a component in an :elements key
(def ui-components {:start-menu [{:id :new-game :pos 0 :type :button :txt "New game" :effect #(char-menu!)}
                                 {:id :about :pos 1 :type :button :txt "About" :effect #(about!)}]
                    :esc-menu [{:id :restart :pos 0 :type :button :txt "Restart" :effect #(restart!)}
                               {:id :about :pos 1 :type :button :txt "About" :effect #(about!)}]
                    ;; elements send data as functions (called by rendering fn)
                    :char-menu [{:id :char-menu :type :panel
                                 :elements [{:id :label :pos 0 :type :button :txt "Choose One"}
                                            {:id :opt1 :pos 1 :type :checkbox :txt "Speed"
                                             :data #(get-in @db [:options :speed])
                                             :effect #(set-option! :speed true :vision false)}
                                            {:id :opt2 :pos 2 :type :checkbox :txt "Vision"
                                             :data #(get-in @db [:options :vision])
                                             :effect #(set-option! :speed false :vision true)}
                                            {:id :ok :pos 3 :type :button :txt " [  OK  ]" :effect #(new-game!)}]}]
                    ;; game-screen component contains a fn that queries world-state
                    :game-screen [{:id :world-map :type :grid :data #(deref (:world-state @db))}]
                    :msg-panel [{:id :msg-pan :type :time-log :pos [40 1]
                                 :data #(deref db)}]})

;; db should contain all info needed for generating the interface
(defonce db (atom {:ui-components ui-components
                   :world-state world-state
                   :keychan dialog-chan
                   :focused [:start-menu 0] :background [] :foreground []
                   :dims [60 40]
                   :options {} :log []}))

(def new-game-state {:keychan key-chan :log []
                     :focused [:game-screen 0] :background [] :foreground [:msg-panel]})

;; ui functions mutate the db
(defn set-option! [& opts]
  (swap! db update :options #(apply assoc % opts))
  nil)

(defn about! []
  (println "about")
  nil)

(defn menu! []
  ;; transfer control to menu
  (swap! db assoc :keychan dialog-chan :focused [:esc-menu 0] :background [:game-screen])
  ;; force re-render to make menu appear
  (render-ui @db)
  ;; opening the menu isn't really an action, so return nil
  nil)

(defn char-menu! []
  (swap! db assoc :focused [:char-menu 0 :elements 0] :background [])
  nil)

;; when this fn is sent via channel to turn-loop, returns 
;;   a result that causes loop to exit
(defn quit-command []
  {:end-game true})

(defn restart! []
  (swap! db assoc :focused [:char-menu 0 :elements 3] :background [:game-screen])
  ;; send quit command to turn-loop
  (put! control-chan quit-command)
  nil)

;; ui controls
(defn move-focus! [i]
  (let [{ui-comps :ui-components key-coll :focused} @db
        comp-key (butlast key-coll)
        idx (last key-coll)
        n (count (get-in ui-comps comp-key))
        x (+ i idx)]
    (swap! db assoc :focused
           (conj (vec comp-key) (cond
                                  (< x 0) (+ x n)
                                  (>= x n) (- x n)
                                  :else x))
           ))
  nil)

(defn click! []
  (let [{ui-comps :ui-components key-vec :focused} @db
        comp (get-in ui-comps key-vec)]
    (when-let [f (:effect comp)]
      (f)
      ))
  nil)


;; translates javascript keyboard event to input code
;; puts it on the channel specified in UI database
;;   the cond-> macro threads its first argument through pairs of statements
;;   applying the 2nd statement if the 1st is true
;;   cond-> [] with conj statements is a way to build up a vector
(defn key-event [e]
  ;(println (. e -keyCode))
  (put! (:keychan @db)
        (cond-> []
          (. e -shiftKey) (conj :shift)
          (. e -ctrlKey) (conj :ctrl)
          :always (conj (. e -keyCode)))))

;;maps input codes to functions for game screen
(def keymap {[37] #(move-player! 6)
             [:shift 37] #(move-player! 7)
             [38] #(move-player! 0)
             [:shift 38] #(move-player! 1)
             [39] #(move-player! 2)
             [:shift 39] #(move-player! 3)
             [40] #(move-player! 4)
             [:shift 40] #(move-player! 5)
             [13] #(open-box!)
             [32] #(open-box!)
             [27] #(menu!)})

;;maps input codes to functions for dialogs/menus
(def dialog-keymap {[37] #(move-focus! -1)
                    [38] #(move-focus! -1)
                    [39] #(move-focus! 1)
                    [40] #(move-focus! 1)
                    [13] #(click!)
                    [32] #(click!)})

;;"runs" in the background to execute player commands only on player turn
(defn control-loop [signal-chan fn-chan]
  (let [out (chan)]
    ;;wait for signal
    (go-loop [_ (<! signal-chan)]
      ;execute commands from fn-chan until non-nil result 
      (loop []
        (if-let [result ((<! fn-chan))]
          (>! out result) (recur)))
      ;;wait for next signal
      (recur (<! signal-chan)))
    ;;return output channel
    out))

;;set up a channel pipeline: player-turn + control-chan => player-result
(defonce player-turn (chan))
(defonce player-result (control-loop player-turn control-chan))

;;take-turn returns a channel containing result
;; multi-method dispatch based on :type key
(defmulti take-turn #(:type %))

;; re-draw the screen at start of player's turn
;; player-result channel will contain a result when control-loop puts one there
(defmethod take-turn :local-player [_]
  (render-ui @db)
  (put! player-turn true)
  player-result)

;;npc result comes from fn contained in :action key
(defmethod take-turn :npc [e]
  (let [out (chan)
        result ((:action e))]
    (if result
      ;; put result on out channel and return it
      (do
        (put! out result)
        out)
      ;; if result is nil, entity did something that did not consume its turn
      (recur e))))

;; turn-loop spawns a looping process that allocates game turns
;;   make sure it's stopped before being called again
(defn turn-loop [scheduler out]
  (go-loop []
    ;; get the entity based on next key in scheduler
    (let [entity (get-in @world-state [:entities (.next scheduler)])
          ;;park the loop here until <! take-turn sends a result
          result (<! (take-turn entity))
          ;; send time-stamped result to game-rules!
          ;; returns true if game should continue
          continue? (game-rules! (assoc result :time (.getTime scheduler)))]
      ;; set action duration in scheduler
      ;; TODO: world should determine all durations
      (.setDuration scheduler (or (:dt result) 10))
      ;; pass result to output channel
      (>! out result)
      ;; continue or exit loop
      (if continue? (recur) (println "exit loop"))
      )))

;; creates a scheduler and adds initial entities, then calls turn-loop
(defn init-turn-loop []
  (let [out (chan)
        scheduler (rot/Scheduler.Action.)]
    (doseq [k (keys (:entities @world-state))] (.add scheduler k true))
    (turn-loop scheduler out)
    ;; monitor turn-loop's output channel
    (go (while true (println (<! out))))
    ))

(defn new-game! []
  ;;transfer control to game screen
  (swap! db merge new-game-state)
  ;;(re)set world state
  (world/reset-state)
  ;;apply options set through UI
  (game-options! (:options @db))
  ;;generate the game world
  (world/init-grid! (:dims @db))
  ;;display has already been init'ed, just reset defaults
  (draw/reset-defaults)
  ;;start the game
  (init-turn-loop))


;; put game rule logic in this fn for now
;;  returns true if game continues
(defn game-rules! [{:keys [time] :as result}]
  ;; log the result
  (swap! db update :log conj {:msg result :time time})
  ;; update world time
  (swap! world-state world/set-time time)
  (cond
    (:ananas result) (do
                       (js/alert "The box contains a golden pineapple, heavy with promise and juice.\n\nIt shines as you hold it aloft. In the distance, Pedro howls in rage.")
                       (swap! draw/context #(assoc-in % [:icons :player] (get-in % [:icons :ananas])))
                       true)
    (= 0 (:path-length result)) (do
                                  (js/alert "Pedro has caught you!\n\nYOU ARE THE NEW PEDRO!")
                                  (swap! draw/context #(assoc-in % [:icons :player] (get-in % [:icons :pedro])))
                                  (swap! world-state assoc-in [:entities :pedro :action] (fn [_] {:pedro :skip}))
                                  true)
    (:end-game result) false
    :else true))


;; put game option logic in this fn for now
(defn game-options! [{:keys [speed vision]}]
  (swap! world-state update-in [:entities :player]
         #(assoc %
                 :fov-fn (if vision :fov-360 :fov-90)
                 :diag (if speed 1.0 1.4))))



;;spawns a process that listens for keyboard codes sent to key-chan
;;  should be called only once, in main!
(defn key-loop []
  (go-loop []
    (when-let [f (get keymap (<! key-chan))]
      ;;TODO: don't put it directly on control-chan
      ;;write keymap functions that feed ctrl-ch
      (>! control-chan f))
    (recur)))

;;spawns a process that listens for keyboard codes sent to dialog-chan
;;  should be called only once, in main!
(defn dialog-loop []
  (go-loop []
    (when-let [f (get dialog-keymap (<! dialog-chan))]
      (f)
      ;;render new ui state after dialog effect
      (render-ui @db))
    (recur)))

;; called when app first loads (as specified in shadow-cljs.edn)
(defn main! []
  (. js/document addEventListener "keydown" key-event)
  (dialog-loop)
  (key-loop)
  (go
    (when (<! (init-disp! (:dims @db)))
      (render-ui @db))))

;; hot reload; called by shadow-cljs when code is saved
;;   object state is preserved
(defn reload! []
  (render-ui @db))
