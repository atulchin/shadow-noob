
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

;; ui components, named by keyword
;;  TODO later: figure out a clean way to send data so you can break ui
;;    out into a separate lib
(declare set-option! new-game! about! char-menu! db)
;; a component is a vector of maps
;;    a compoment can contain a component in an :elements key
(def ui-components {:start-menu [{:id :new-game :pos 0 :type :button :txt "New game" :effect #(char-menu!)}
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
                                 :data #(deref db)}]
                    })

;; db should contain all info needed for generating the interface
(defonce db (atom {:ui-components ui-components
                   :world-state world-state
                   :keychan dialog-chan
                   :focused [:start-menu 0] :background [] :foreground []
                   :dims [60 40]
                   :options {} :log [] :time 0}))

(def new-game-state {:keychan key-chan :time 0 :log []
                     :focused [:game-screen 0] :background [] :foreground [:msg-panel]})

;; ui functions mutate the db
(defn set-option! [& opts]
  (swap! db update :options #(apply assoc % opts)))

(defn about! []
  (println "about"))

(defn menu! []
  ;; transfer control to menu
  (swap! db assoc :keychan dialog-chan :focused [:start-menu 0] :background [:game-screen])
  ;; force re-render to make menu appear
  (render-ui @db)
  ;; opening the menu isn't really an action, so return nil
  nil)

(defn char-menu! []
  (swap! db assoc :focused [:char-menu 0 :elements 0] :background []))

(defn quit! []
  ;; transfer control to menu
  (swap! db assoc :keychan dialog-chan :focused [:start-menu 0] :background [:game-screen])
  ;; force re-render to make menu appear
  (render-ui @db)
  ;;returns a result that stops the turn loop
  {:end-game true})

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
                                  :else x)))
    ))

(defn click! []
  (let [{ui-comps :ui-components key-vec :focused} @db
        comp (get-in ui-comps key-vec)]
    (when-let [f (:effect comp)]
      (f)
      )))


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
             [27] #(menu!)
             [:shift 81] #(quit!)})

;;maps input codes to functions for dialogs/menus
(def dialog-keymap {[37] #(move-focus! -1)
                    [38] #(move-focus! -1)
                    [39] #(move-focus! 1)
                    [40] #(move-focus! 1)
                    [13] #(click!)
                    [32] #(click!)})

;;called on player's turn to process game-screen input
(defn handle-input [code]
  ;(println code)
  (when-let [f (get keymap code)]
    (f)))

;;spawns a process that listens for keyboard codes sent to dialog-chan
;;called only once, in main!
(defn dialog-loop []
  (go-loop []
    (let [code (<! dialog-chan)
          f (get dialog-keymap code)]
      (when f (f))
      ;;render new ui state after every dialog input
      (render-ui @db)
      (recur))))

;; turn-loop spawns a looping process that allocates game turns
;; called from new-game!
;;   make sure it's stopped before being called again
(defn turn-loop []
  (let [out (chan)
        scheduler (rot/Scheduler.Action.)]
    ;; add initial entities to scheduler
    (doseq [k (keys (:entities @world-state))] (.add scheduler k true))

    ;;get next key in schduler
    ;;  also keep track of whether to re-draw the screen
    (go-loop [entity-key (.next scheduler)
              re-render true]
      ;; get the entity based on key from scheduler
      ;;   the scheduler should never be empty
      ;;   but if it is, this loop will gracelessly terminate
      (when-let [entity (get-in @world-state [:entities entity-key])]
        ;; could put this in separate functions, but then waiting
        ;;    for keyboard input would have to run in a nested go loop
        (let [result (case (:type entity)
                       ;;player result = fn called by handle-input
                       ;; also re-draw the screen at start of player's turn
                       :local-player (do (when re-render (render-ui @db))
                                         ;; <! key-chan will park waiting for key input
                                         (handle-input (<! key-chan)))
                       ;;npc result = fn contained in :action key
                       :npc ((:action entity))
                       ;;default value
                       {})]
          ;; if result is nil, entity did something that did not consume its turn
          (if (nil? result)
            (recur entity-key false)
            ;; otherwise process the result
            ;;   apply game rules based on the result
            ;;   also determines whether to continue the game
            (let [continue? (game-rules! result)
                  ;; get elapsed time from result (or default duration)
                  ;;   TODO: world should track all action durations
                  dt (or (:dt result) 10)
                  ;t' (+ t dt)
                  t' (.getTime scheduler)]
              ;; set action duration in scheduler
              (.setDuration scheduler dt)
              ;; log the result
              ;;  TODO move this to game rules?
              (swap! db #(-> %
                             (update :log conj {:msg #_(dissoc result :dt) (assoc result :time t') :time t'})
                             (assoc :time t')))
              ;;pass result to output channel
              (>! out result)
              ;; continue with next entity in scheduler
              (when continue? (recur (.next scheduler) true))
              )))))
    ;;return output channel
    out))

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
  (let [debug-ch (turn-loop)]
    ;; monitor turn-loop's output channel
    (go (while true (println (<! debug-ch))))
    ))



;; put game rule logic in this fn for now
;;  returns true if game continues
(defn game-rules! [result]
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
    :else true
    ))


;; put game option logic in this fn for now
(defn game-options! [{:keys [speed vision]}]
  (swap! world-state update-in [:entities :player]
         #(assoc %
                 :fov-fn (if vision :fov-360 :fov-90)
                 :diag-time (if speed 10 14))))



;; called when app first loads (as specified in shadow-cljs.edn)
(defn main! []
  (. js/document addEventListener "keydown" key-event)
  (dialog-loop)
  (go
    (when (<! (init-disp! (:dims @db)))
      (render-ui @db))))

;; hot reload; called by shadow-cljs when code is saved
;;   object state is preserved
(defn reload! []
  (render-ui @db))
