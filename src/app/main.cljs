
(ns app.main
  (:require ["rot-js" :as rot]
            [clojure.core.async :as a :refer [>! <! put! go go-loop chan dropping-buffer]]
            [app.world :as world :refer [world-state move-player! open-box! player-item!]]
            [app.drawcanvas :as draw :refer [init-disp! render-ui]]))

;; this file is for manipulating the user inteface & interacting w/ game state
;; TODO: game logic also lives here for now, may move it
(declare game-rules! game-options!)

;;channels for processing input
;;  key-chan is read by the main game screen
(defonce key-chan (chan (dropping-buffer 5)))
;;  dialog-chan is read by ui dialogs/menus
(defonce dialog-chan (chan (dropping-buffer 5)))
;;  targeting interface: target-ch for reading input
;;    select-ch for sending selection
(defonce target-chan (chan (dropping-buffer 5)))
(defonce select-chan (chan))
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
                    :game-screen [{:id :world-map :type :grid
                                   :data (fn []
                                           (let [{:keys [focused target world-ref]} @db
                                                 s @world-ref]
                                             {:world-state s
                                              :focal-coords (if (= (first focused) :target-overlay)
                                                              target
                                                              (get-in s [:entities :player :coords]))}
                                             ))
                                   }]
                    :msg-panel [{:id :msg-pan :type :time-log :pos [40 1] :data #(:log @db)}]
                    :target-overlay [{:id :cursor :type :cursor :data #(:target @db)}
                                     {:id :info :type :target-info :pos [40 38] :data #(world/get-info (:target @db))}]
                    })

;; db should contain all info needed for generating the interface
(defonce db (atom {:ui-components ui-components
                   :world-ref world-state
                   :keychan dialog-chan
                   :focused [:start-menu 0] :background [] :foreground []
                   :map-dims [60 40] :screen-dims [60 40] :target [0 0]
                   :options {} :log []
                   :running false}))

(def new-game-state 
  {:keychan key-chan :focused [:game-screen 0] :background [] 
   :foreground [:msg-panel]
   :log [] :running true})

;; menu text for items defined in world/items
(def item-text {:potion-speed "Potion of speed"
                :scroll-teleport "Scroll of teleport"})

;; ui functions triggered from dialogs/menus; mutate the db
(defn set-option! [& opts]
  (swap! db update :options #(apply assoc % opts))
  nil)

(defn about! []
  (println "about")
  nil)

(defn char-menu! []
  (swap! db assoc :focused [:char-menu 0 :elements 0] :background [])
  nil)

(defn restart! []
  ;; open character menu
  (swap! db assoc :focused [:char-menu 0 :elements 3] :background [:game-screen])
  nil)

(defn game-screen! []
  (swap! db assoc :keychan key-chan :focused [:game-screen 0] :background []))

;; ui controls
(defn move-focus [s i]
  (let [{ui-comps :ui-components key-coll :focused} s
        comp-key (butlast key-coll)
        idx (last key-coll)
        n (count (get-in ui-comps comp-key))
        x (+ i idx)]
    (assoc s :focused
           (conj (vec comp-key) (cond
                                  (< x 0) (+ x n)
                                  (>= x n) (- x n)
                                  :else x))
           )))

(defn move-focus! [i]
  (swap! db move-focus i)
  nil)

;; executes :effect fn defined in ui element
(defn click! []
  (let [{ui-comps :ui-components key-vec :focused} @db
        comp (get-in ui-comps key-vec)]
    (when-let [f (:effect comp)]
      (f)
      ))
  nil)

(defn move-target [s [x y]]
  (update s :target
          #(->> %
                (mapv + [x y])
                (mapv min (:map-dims s))
                (mapv max [0 0])
                )))

(defn move-target! [[x y]]
  (swap! db move-target [x y])
  nil)

(defn select-target! []
  (let [targ-info (world/get-info (:target @db))]
    ;; only allow targeting seen/visible grid squares
    (when (and (:seen? targ-info) (:grid targ-info))
      ;; send target info to select-ch
      (put! select-chan targ-info)
      ;; give control to game screen
      (game-screen!)))
  nil)

(defn close-menu! []
  ;; TODO - need to keep track of previous state
  ;; for now, just give control to game screen
  (when (:running @db)
    (game-screen!))
  nil)

(defn cancel-target! []
  ;; send empty record to select-ch
  (put! select-chan {})
  ;; TODO - need to keep track of previous state
  ;; for now, just give control to game screen
  (game-screen!)
  nil)

(defn target-ui! []
  (swap! db assoc
         :target (get-in @world-state [:entities :player :coords])
         :keychan target-chan :focused [:target-overlay 0] :background [:game-screen]))

;; called when an inventory item is clicked
(defn inv-click! [k]
  ;; if item requires target:
  (if (:target (meta (get world/items k)))
    (do
      ;; transfer control to targeting interface
      (target-ui!)
      ;; spawn process that will wait for target info
      (go (let [targ-info (<! select-chan)]
            ;; if target provided, send use-item command to turn loop 
            (when (seq targ-info) (put! control-chan #(player-item! k targ-info)))
            )))
    ;; else, item doesn't require target:
    (do
      ;; give control back to game screen
      (game-screen!)
      ;; send use-item command to turn loop, with empty map as target info
      (put! control-chan #(player-item! k {}))
      ))
  nil)

;; generate a menu from an inventory hashmap
(defn inv-comp [m]
  (conj (into [{:id :label :pos 0 :type :button :txt "-- Items --"}]
              (map-indexed (fn [i [k v]]
                             {:id k :pos (inc i) :type :button :txt (str (item-text k) " x" v) :effect #(inv-click! k)})
                           m))
        {:id :cancel :pos (inc (count m)) :type :button :txt "[  Cancel  ]" :effect #(close-menu!)}))

;; ui functions triggered from game screen on player's turn
(defn inventory-menu! []
  ;;add/update menu in db
  (swap! db update :ui-components assoc :inv-menu
         (inv-comp (get-in @world-state [:entities :player :inv])))
  ;; transfer control to menu
  (swap! db assoc :keychan dialog-chan :focused [:inv-menu 0] :background [:game-screen])
  ;; force re-render to make menu appear
  (render-ui @db)
  ;; opening the menu isn't really an action, so return nil
  nil)

(defn menu! []
  ;; transfer control to menu
  (swap! db assoc :keychan dialog-chan :focused [:esc-menu 0] :background [:game-screen])
  ;; force re-render to make menu appear
  (render-ui @db)
  ;; opening the menu isn't really an action, so return nil
  nil)

(defn look! []
  ;; set target to player coords
  ;; transfer control to targeting interface
  (target-ui!)
  ;; force re-render
  (render-ui @db)
  ;; spawn process that will read target selection
  ;;   just send to log for now
  (go (let [{:keys [coords seen? visible? grid entities time]} (<! select-chan)]
        (when seen?
          (swap! db update :log conj
                 {:msg (str coords " " (when seen? grid) " " (when visible? entities))
                  :time time})
          (render-ui @db))
        ))
  ;; not an action, return nil  
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
             [27] #(menu!)
             [76] #(look!)
             [73] #(inventory-menu!)})

;;maps input codes to functions for dialogs/menus
(def dialog-keymap {[37] #(move-focus! -1)
                    [38] #(move-focus! -1)
                    [39] #(move-focus! 1)
                    [40] #(move-focus! 1)
                    [13] #(click!)
                    [32] #(click!)
                    [27] #(close-menu!)})

;;maps input codes to functions for target selection
(def target-keymap {[37] #(move-target! [-1 0])
                    [:shift 37] #(move-target! [-1 -1])
                    [38] #(move-target! [0 -1])
                    [:shift 38] #(move-target! [1 -1])
                    [39] #(move-target! [1 0])
                    [:shift 39] #(move-target! [1 1])
                    [40] #(move-target! [0 1])
                    [:shift 40] #(move-target! [-1 1])
                    [13] #(select-target!)
                    [32] #(select-target!)
                    [27] #(cancel-target!)})


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
(defmulti take-turn :type)

;; re-draw the screen at start of player's turn
;; player-result channel will contain a result when control-loop puts one there
(defmethod take-turn :local-player [_]
  (render-ui @db)
  ;; TODO - maybe use this channel to pass entity info to control loop?
  ;;   since it's already passed to this function by turn loop
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

;; when this fn is sent via channel to turn-loop, returns 
;;   a result that causes loop to exit
(defn quit-command []
  {:end-game true})

(defn new-game! []
  ;; if already running, send quit command to turn-loop
  (when (:running @db) (put! control-chan quit-command))
  ;;display has already been init'ed, just reset defaults
  (draw/reset-defaults)
  ;;transfer control to game screen
  (swap! db merge new-game-state)
  ;;(re)set world state
  (world/reset-state)
  ;;apply options set through UI
  (game-options! (:options @db))
  ;;generate the game world
  (world/init-grid! (:map-dims @db))
  ;;update drawing boundaries
  ;;TODO - move this and map creation to a separate fn
  (draw/set-breakpts! (draw/calc-breakpoints (:map-dims @db) (:screen-dims @db)))
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

;;spawns a process that listens for keyboard codes sent to target-chan
;;  should be called only once, in main!
(defn target-loop []
  (go-loop []
    (when-let [f (get target-keymap (<! target-chan))]
      (f)
      ;;render new ui state to update view
      (render-ui @db))
    (recur)))

;; called when app first loads (as specified in shadow-cljs.edn)
(defn main! []
  (. js/document addEventListener "keydown" key-event)
  (dialog-loop)
  (key-loop)
  (target-loop)
  (go
    (when (<! (init-disp! (:screen-dims @db)))
      (render-ui @db))))

;; hot reload; called by shadow-cljs when code is saved
;;   object state is preserved
(defn reload! []
  (render-ui @db))
