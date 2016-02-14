(ns insane-carnage.game
  (:require [reagent.core :as reagent :refer [atom]]
            [accountant.core :as accountant]
            [clojure.string :as str]
            [insane-carnage.db :refer [db]]
            [insane-carnage.talk :refer [chsk-send!]]
            [cljs-time.core :as time]
            [insane-carnage.engine :as engine]))

(defn- clear-game-loop! [db]
  (if-let [timer (:timer db)]
    (do
      (js/clearInterval timer)
      (dissoc db :timer))
    db))

(defn- game-loop []
  (swap! db
         (fn [db]
           (let [game (:game db)
                 [next-game _] (engine/process-tick game (inc (:tick game)))]
             (when (not= :running (:state next-game))
               (clear-game-loop! db))
             (assoc db :game next-game)))))

(defn- start-game-loop! [db]
  (clear-game-loop! db)
  ;(assoc db :timer (js/setInterval game-loop engine/tick-duration))
  db
  )

(defn pause []
  (swap! db
         (fn [db]
           (let [next-db (update db :pause not)]
             (if (:pause db)
               (start-game-loop! next-db)
               (clear-game-loop! next-db))))))

(defn- joined! [game unit-id log]
  {:pre [unit-id]}
  (swap! db
         #(-> %
              (assoc
                :game-id (:id game)
                :game game
                :unit-id unit-id
                :state :running
                :log log)
              (start-game-loop!)))
  (accountant/navigate! (str "/game/" (:id game))))

(defn join! [game-id]
  (.log js/console "Join game" game-id)
  (chsk-send! [:game/join {:player-name (:player-name @db)
                           :player-id   (:player-id @db)
                           :game-id     game-id}])
  (swap! db
         #(-> %
              (assoc :state :waiting)
              (assoc :game-id game-id))))

(defn change-unit! []
  (join! (:game-id @db)))

(defn reset []
  (swap! db
         #(-> %
              (clear-game-loop!)
              (assoc :state :setup)
              (dissoc :game-id)
              (dissoc :game)
              (dissoc :unit-id))))

(defn leave! []
  (println "Leave game")
  (chsk-send! [:game/leave])
  (accountant/navigate! "/"))

(defn player-move [move]
  (let [unit-id (:unit-id @db)]
    (when-not (= move (get-in @db [:game :units unit-id :move]))
      (println "Move" move)
      (swap! db update :game engine/set-unit-move (:unit-id @db) move)
      (chsk-send! [:player/move {:move move}]))))

(defn update-game! [game log]
  (swap! db
         #(-> %
              (assoc :game game)
              (update :log concat log))))