(ns insane-carnage.game
  (:require [clojure.tools.logging :as log]
            [insane-carnage.engine :as engine]
            [clojure.string :as str]
            [clj-time.core :as time]
            [clj-time.format :as time-format]))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(def init-state
  {:games   {}
   :players {}})

(def server-state
  (atom init-state))

(defn reset-all! []
  (reset! server-state init-state))

(def valid-directions
  (for [x (range 3)
        y (range 3)
        :when (not (and (= 1 x)
                        (= 1 y)))]
    [(dec x) (dec y)]))

(defn random-direction []
  (rand-nth valid-directions))

(defn- generate-players [width height cnt]
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
                    :direction (random-direction)
                    :state     :moving
                    :type      :swardsman}])))
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
   (let [width 100
         height 100
         ts (time/now)]
     {:game-id game-id
      :width   width
      :height  height
      :sight   15
      :players (generate-players width height 30)
      :game-started (str ts)
      :log     [{:ts      (format-log-time ts ts)
                 :message "Game started"}]
      })))

(defn- ensure-player [state player-id player-name]
  (assoc-in state [:players player-id] {:player-id   player-id
                                        :player-name player-name}))

(defn find-available-game-player [state game-id]
  (let [players (get-in state [:games game-id :players])]
    (->> players
         (vals)
         (filter #(not (:player-id %)))
         (first))))

(defn- bind-player-to-game [state player-id game-id]
  (let [game-player (find-available-game-player state game-id)]
    (log/info "Bind player" player-id "to game" game-id "player" game-player)
    (-> state
        (assoc-in [:players player-id :game-id] game-id)
        (assoc-in [:players player-id :game-player-id] (:id game-player))
        (assoc-in [:games game-id :players (:id game-player) :player-id]
                  player-id))))

(defn- choose-random-game []
  (-> (:games @server-state) vals rand-nth))

(defn- get-game [game-id]
  (get-in @server-state [:games game-id]))

(defn start-new-game [player-id player-name]
  (let [game (generate-game)
        game-id (:game-id game)]
    (log/info "Player" player-id "(" player-name ") started new game" game-id)
    (swap! server-state
           #(-> %
                (assoc-in [:games game-id] game)
                (ensure-player player-id player-name)
                (bind-player-to-game player-id game-id)))
    (get-in @server-state [:games game-id])))

(defn join-random-game [player-id player-name]
  (log/info "Player" player-id "(" player-name ") want to join a random game")
  (let [game (or
               (log/spyf "Choose random game: "
                         (choose-random-game))
               (log/spyf "No game to choose from. Create new game."
                         (generate-game)))
        game-id (:game-id game)]
    (swap! server-state
           #(-> %
                (assoc-in [:games game-id] game)
                (ensure-player player-id player-name)
                (bind-player-to-game player-id game-id)))
    game))

(defn join-or-create-game [player-id player-name game-id]
  (log/info "Player" player-id "(" player-name ") want to join game" game-id)
  (let [game (or
               (log/spyf (str "Join game " game-id)
                         (get-game game-id))
               (log/spyf "Game doesn't exist. Create new game."
                         (generate-game game-id)))]
    (swap! server-state
           #(-> %
                (ensure-player player-id player-name)
                (bind-player-to-game player-id game-id)))
    game))

(defn unbind-player-from-game [player-id]
  (if-let [player (get-in @server-state [:players player-id])]
    (let [{:keys [game-id game-player-id]} player]
      (log/info "Unbind player" player-id "from game" game-id "(game player " game-player-id ")")
      (swap! server-state
             #(-> %
                  (dissoc [:players player-id :game-id])
                  (dissoc [:players player-id :game-player-id])
                  (dissoc [:games game-id :players game-player-id :player-id]))))))

(defn process-direction [player-id dir add?]
  (let [player (get-in @server-state [:players player-id])
        game (get-in @server-state [:games (:game-id player)])]
    (if add?
      nil                                                   ;(engine/add-direction game player dir)
      nil                                                   ;(engine/remove-direction game player dir)
      )))