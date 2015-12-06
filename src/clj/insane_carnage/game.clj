(ns insane-carnage.game
  (:require [clojure.tools.logging :as log]
            [clojure.core.async :refer [go-loop go <! >! chan close! put! to-chan]]
            [insane-carnage.engine :as engine]
            [clojure.string :as str]
            [clj-time.core :as time]
            [clj-time.format :as time-format]
            [clojure.data :refer [diff]]))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(def init-state
  {:games   {}
   :players {}})

;(def server-state
;  (atom init-state))
(def players (atom {}))

(defonce update-chan (chan))

(defn reset-all! []
  nil
  ;(reset! server-state init-state)
  )

(def valid-directions
  (for [x (range 3)
        y (range 3)
        :when (not (and (= 1 x)
                        (= 1 y)))]
    [(dec x) (dec y)]))

(def unit-types [:swordsman
                 :pikeman])

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
                   {:x              x
                    :y              y
                    :id             id
                    :hp             100
                    :max-hp         100
                    :last-hit-at    0
                    :direction      (rand-nth valid-directions)
                    :move-direction (rand-nth [-1 1])
                    :state          (rand-nth [:moving :staying])
                    :type           (rand-nth unit-types)}])))
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
     {:game-id      game-id
      :width        width
      :height       height
      :sight        15
      :players      (generate-players width height 30)
      :game-started (str ts)
      :tick         0
      :status       :in-progress
      :log          [{:ts      (format-log-time ts ts)
                      :message "Game started"}]
      })))

(defn- ensure-player [players player-id player-name]
  (assoc players player-id {:player-id   player-id
                            :player-name player-name}))

(defn find-available-game-player [game]
  (let [players (:players game)]
    (->> players
         (vals)
         (filter #(not (:player-id %)))
         (first))))

(defn- bind-player-to-game [games game-player player-id game-id]
  (assoc-in games [game-id :players (:id game-player) :player-id] player-id))

(defn- bind-game-to-player [players game-player player-id game-id]
  (update-in players
             [player-id]
             assoc
             :game-id game-id
             :game-player-id (:id game-player)))

(defn- choose-random-game []
  (-> @engine/games vals rand-nth))

(defn- get-game [game-id]
  (get @engine/games game-id))

(defn game-live-player-ids [game-id]
  (->> (get @engine/games game-id)
       (:players)
       (vals)
       (filter :player-id)
       (map :player-id)))

(defn update-games-and-players [game player-id player-name]
  (let [game-id (:game-id game)
        game-player (find-available-game-player game)]
    (swap! engine/games
           #(-> %
                (assoc game-id game)
                (bind-player-to-game game-player player-id game-id)))
    (swap! players
           #(-> %
                (ensure-player player-id player-name)
                (bind-game-to-player game-player player-id game-id)))))

(defn game-updated [game-id update]
  ())

(defn register-game [game]
  (let [{:keys [game-id]} game
        out-events (get-in @engine/game-channels [game-id :out])]
    (engine/register-game game-id)
    (engine/game-loop game-id)
    (go-loop []
      (when-let [msg (<! out-events)]
        (when-let [update (:update msg)]
          (game-updated (:game-id msg) update))))))

(defn start-new-game [player-id player-name]
  (let [game (generate-game)
        game-id (:game-id game)
        game-player (find-available-game-player game)]
    (log/info "Player" player-id "(" player-name ") started new game" game-id)
    (update-games-and-players game player-id player-name)
    (register-game game)
    (get-game game-id)))

(defn join-random-game [player-id player-name]
  (log/info "Player" player-id "(" player-name ") want to join a random game")
  (let [game (or
               (log/spyf "Choose random game: "
                         (choose-random-game))
               (log/spyf "No game to choose from. Create new game."
                         (generate-game)))
        game-id (:game-id game)]
    (update-games-and-players game player-id player-name)
    (get-game game-id)))

(defn join-or-create-game [player-id player-name game-id]
  (log/info "Player" player-id "(" player-name ") want to join game" game-id)
  (let [game (or
               (log/spyf (str "Join game " game-id)
                         (get-game game-id))
               (log/spyf "Game doesn't exist. Create new game."
                         (generate-game game-id)))]
    (update-games-and-players game player-id player-name)
    (get-game game-id)))

(defn unbind-player-from-game [player-id]
  (if-let [player (get @players player-id)]
    (let [{:keys [game-id game-player-id]} player]
      (log/info "Unbind player" player-id "from game" game-id "(game player " game-player-id ")")
      (swap! engine/games
             #(-> %
                  (dissoc game-id :players game-player-id :player-id)))
      (swap! players
             #(-> %
                  (dissoc player-id :game-id)
                  (dissoc player-id :game-player-id))))))

(defn process-action [player-id action]
  (let [player (get @players player-id)
        game (get-game (:game-id player))]
    (engine/process-action game player action)))