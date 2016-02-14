(ns insane-carnage.game
  (:require [clojure.tools.logging :as log]
            [clojure.core.async :refer [go-loop go <! >! chan close! put! to-chan pub sub unsub] :as async]
            [insane-carnage.engine :as engine]
            [clojure.string :as str]
            [clj-time.core :as time]
            [medley.core :refer :all]
            [clojure.data :refer [diff]]
            [chime :refer [chime-ch]]
            [clj-time.core :as time]
            [clj-time.periodic :as periodic]
            [taoensso.truss :as truss :refer (have have! have?)]
            ))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(def valid-directions
  (for [x (range 3)
        y (range 3)
        :when (not (and (= 1 x)
                        (= 1 y)))]
    [(dec x) (dec y)]))

(defn- generate-players [width height cnt tick]
  (let [all-positions (for [w (range width)
                            h (range height)] [w h])
        positions (->> all-positions
                       (shuffle)
                       (take cnt))]
    (->> positions
         (map (fn [[x y]]
                (let [id (uuid)]
                  [id
                   {:x         x
                    :y         y
                    :id        id
                    :hp        100
                    :max-hp    100
                    :direction (rand-nth valid-directions)
                    :move      (-> engine/possible-moves
                                   (disj :hit)
                                   (seq)
                                   (rand-nth))
                    :moved-at  tick
                    :hit       nil
                    :type      (-> engine/unit-types keys rand-nth)}])))
         (shuffle)
         (into {}))))

(defn- pad-time [v]
  (str/join (take-last 2 (str "00" v))))

(defn- format-log-time [game-started ts]
  (let [secs (time/in-seconds (time/interval game-started ts))]
    (str (pad-time (quot secs 60)) ":"
         (pad-time (mod secs 60)))))

(defn- generate-game
  ([] (generate-game (uuid)))
  ([game-id]
   (let [width 30
         height 30
         ts (time/now)]
     {:id         game-id
      :width      width
      :height     height
      :sight      15
      :units      (generate-players width height 30 0)
      :started-at (str ts)
      :tick       0
      :state      :running
      :log        [{:ts      (format-log-time ts ts)
                    :message "Game started"}]
      })))

;; -------------------------
;; Utility

(defn- unit-dead? [unit]
  (-> unit :hp pos? not))

(defn- game-full? [game]
  (->> (:units game)
       (remove-vals :player-id)
       (empty?)))

(defn- random-available-game [games]
  (->> games
       (vals)
       (remove game-full?)
       (first)))

(defn- random-available-unit [game]
  (->> (:units game)
       (vals)
       (remove unit-dead?)
       (first)))

;; -------------------------
;; Channel based game

(defn archive-game! [game]
  (log/infof "Game %s archived" (:id game)))

(defmulti game-event
          (fn [msg game ch-out] (:type msg)))

(defmethod game-event :game/start [_ game ch-out]
  (let [next-game (assoc game :state :running)]
    (put! ch-out {:type :game/started
                  :game next-game})
    next-game))

(defmethod game-event :game/join [{:keys [player-id] :as msg} game ch-out]
  (let [unit (random-available-unit game)
        next-unit (assoc unit :player-id player-id)
        next-game (assoc-in game [:units (:id unit)] next-unit)]
    (log/info "game-event" msg unit next-unit)
    (put! ch-out {:type      :game/joined
                  :game      next-game
                  :unit      next-unit
                  :player-id player-id})
    next-game))

(defmethod game-event :game/leave [{:keys [player]} game ch-out]
  (let [next-game (update-in game [:units (:unit-id player)] dissoc :player-id)]
    next-game))

(defmethod game-event :game/tick
  [{:keys [tick] :as msg} game ch-out]
  (let [[next-game log] (engine/process-tick game tick)
        over? (= :finished (:state next-game))]
    (log/debug "game-event" msg)
    (cond
      over?
      (put! ch-out {:type :game/over
                    :game next-game
                    :log log})
      (not= game next-game)
      (put! ch-out {:type :game/updated
                    :game next-game
                    :log log}))
    next-game))

(defmethod game-event :unit/move
  [{:keys [unit-id move] :as msg} game _]
  {:pre [unit-id (engine/possible-moves move)]}
  (log/info "game-event" msg)
  (engine/set-unit-move game unit-id move))

(defn new-game
  [game-id]
  (let [ch-in (chan)
        ch-out (chan)
        game (assoc (generate-game game-id)
               :ch-in ch-in
               :ch-out ch-out)]
    (go-loop [game game]
      (when-let [msg (<! ch-in)]
        (recur (game-event msg game ch-out))))
    game))

;; -------------------------
;; Channel based server

(def empty-server {:games   {}
                   :players {}
                   :logs    {}
                   :tick    0
                   :pause   false})

(defmulti server-event
          (fn [msg app-state ch-out] (:type msg)))

(defmethod server-event :game/join
  [{:keys [game-id player-id player-name] :as msg} app-state _]
  (let [game (get-in app-state [:games game-id])]
    (log/info "server-event" msg)
    (put! (:ch-in game) {:type      :game/join
                         :player-id player-id})
    (assoc-in app-state [:players player-id]
              {:id   player-id
               :name player-name})))

(defmethod server-event :game/new
  [{:keys [game-id player-id player-name] :as msg} app-state {:keys [mix-in] :as chans}]
  (let [game (new-game game-id)
        next-state (assoc-in app-state [:games game-id] game)]
    (log/info "server-event" msg)
    (async/admix mix-in (:ch-out game))
    (server-event {:type        :game/join
                   :game-id     (:id game)
                   :player-id   player-id
                   :player-name player-name}
                  next-state chans)))

(defmethod server-event :game/join-random
  [{:keys [player-id player-name]} app-state chans]
  (let [game (random-available-game (:games app-state))]
    (server-event {:type        :game/join
                   :game-id     (:id game)
                   :player-id   player-id
                   :player-name player-name}
                  app-state chans)))

(defmethod server-event :game/joined
  [{:keys [game unit player-id] :as msg} app-state {:keys [ch-out]}]
  (log/info "server-event" (dissoc msg :game) (:id game))
  (put! ch-out (assoc msg
                 :log (get-in app-state [:logs (:id game)])))
  (-> app-state
      (update-in [:players player-id] assoc
                 :game-id (:id game)
                 :unit-id (:id unit))
      (assoc-in [:games (:id game)] game)))

(defmethod server-event :game/leave
  [{:keys [player-id]} app-state _]
  (let [player (get-in app-state [:players player-id])
        game (get-in app-state [:games (:game-id player)])
        next-state (-> app-state
                       (update-in [:players player-id] dissoc :unit-id :game-id))]
    (when game
      (put! (:ch-in game) {:type   :game/leave
                           :player player}))
    next-state))

(defmethod server-event :game/updated
  [{:keys [game log] :as msg} app-state {:keys [ch-out]}]
  (log/debug "server-event" (dissoc msg :game) (:id game))
  (put! ch-out msg)
  (-> app-state
      (assoc [:games (:id game)] game)
      (update-in [:logs (:id game)] concat)))

(defmethod server-event :game/log
  [{:keys [log game-id] :as msg} app-state {:keys [ch-out]}]
  (log/debug "server-event" (dissoc msg :log))
  (put! ch-out msg)
  (update-in app-state [:logs game-id] concat log))

(defmethod server-event :game/over
  [{:keys [game]} app-state {:keys [mix-in]}]
  (let []
    (async/unmix mix-in (:ch-out game))
    (archive-game! game)
    (-> app-state
        (dissoc-in [:games (:id game)]))))

(defmethod server-event :player/move
  [{:keys [player-id move] :as msg} app-state _]
  {:pre [(have? [:el engine/possible-moves] move)]}
  (let [player (get-in app-state [:players player-id])
        game (get-in app-state [:games (:game-id player)])]
    (put! (:ch-in game) {:type    :unit/move
                         :unit-id (:unit-id player)
                         :move    move}))
  app-state)

(defmethod server-event :server/tick
  [_ app-state _]
  (let [tick (inc (:tick app-state))]
    (->> (:games app-state)
         (filter-vals #(= :running (:state %)))
         (map-vals #(put! (:ch-in %) {:type :game/tick
                                      :tick tick}))
         (doall))
    (assoc app-state :tick tick)))

(defmethod server-event :server/pause
  [_ app-state _]
  (update app-state :pause not))

(defn- close-games [games]
  (->> games
       (map-vals
         (fn [game]
           (close! (:ch-in game))
           (close! (:ch-out game))))
       (doall)))

(defn new-server
  []
  (let [ch-in (chan)
        mix-in (async/mix ch-in)
        ch-out (chan)
        timer (chime-ch
                (rest
                  (periodic/periodic-seq (time/now)
                                         (time/millis engine/tick-duration))))]
    (go-loop [app-state empty-server]
      (if-let [msg (<! ch-in)]
        (recur
          (if (:pause app-state)
            app-state
            (server-event msg app-state {:ch-out ch-out
                                         :mix-in mix-in})))
        (do
          (close-games (:games app-state))
          (close! ch-in)
          (close! ch-out))))
    (go-loop []
      (when-let [_ (<! timer)]
        (>! ch-in {:type :server/tick})
        (recur)))
    [ch-in ch-out]))