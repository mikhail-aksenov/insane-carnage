(ns insane-carnage.game
  (:require [clojure.tools.logging :as log]))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(def server-state
  (atom
    {:games {}
     :players {}}))

(defn- generate-game []
  {:game-id (uuid)})

(defn- create-player [player-id player-name]
  {:player-id player-id
   :player-name player-name})

(defn- ensure-player [state player-id player-name]
  (assoc-in state [:players player-id] {:player-id player-id
                                        :player-name player-name}))

(defn- bind-player-to-game [state player-id game-id]
  (assoc-in state [:players player-id :game-id] game-id))

(defn- choose-random-game []
  (-> (:games @server-state) vals rand-nth))

(defn start-new-game [player-id player-name]
  (let [game (generate-game)
        game-id (:game-id game)]
    (log/info "Player" player-id "(" player-name ") started new game" game-id)
    (swap! server-state
           #(-> %
                (assoc-in [:games game-id] game)
                (ensure-player player-id player-name)
                (bind-player-to-game player-id game-id)))))

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
                (bind-player-to-game player-id game-id)))))

(defn join-game [player-id player-name game-id]
  (log/info "Player" player-id "(" player-name ") want to join game" game-id)
  (swap! server-state
         #(-> %
              (ensure-player player-id player-name)
              (bind-player-to-game player-id game-id))))