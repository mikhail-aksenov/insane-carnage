(ns insane-carnage.welcome
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [insane-carnage.db :refer [db]]
            [insane-carnage.game :as game]))

(defn start-new-game []
  (accountant/navigate! "/game/new")
  ;(game/join "new")
  )

(defn join-random-game []
  (accountant/navigate! "/game/random")
  ;(game/join "random")
  )

(defn join-game []
  (accountant/navigate! (str "/game/" (:game-id @db)))
  ;(game/join (:game-id @db))
  )

(defn error-msg []
  (let [e (:error @db)]
    ;(when e
    ;  (js/setInterval #(swap! db dissoc :error) 5000))
    (fn []
      [:div.form-group
       (when e
         [:div (str "Sorry. " e)])])))

(defn welcome []
  (let [can-start? (not (empty? (:player-name @db)))]
    [:div#welcome
     [:h1 "Insane carnage"]
     [:div.form-group
      [:label "Your name (required)"]
      [:input.form-control {:type      "text"
                            :value     (:player-name @db)
                            :on-change #(swap! db
                                               assoc
                                               :player-name
                                               (.. % -target -value))}]]
     [:div.form-group
      [:button.btn.btn-default {:disabled (not can-start?)
                                :on-click start-new-game}
       "Start new game"]]
     [:div.form-group
      [:button.btn.btn-default {:disabled (not can-start?)
                                :on-click join-random-game}
       "Join random game"]]
     [:div.form-group
      [:div.row
       [:div.col-xs-4
        [:div.btn.btn-default {:disabled (or
                                           (not can-start?)
                                           (empty? (:game-id @db)))
                               :on-click join-game}
         "Join"]]
       [:div.col-xs-2 {:style {:padding "0.5rem 0"}} "game #"]
       [:div.col-xs-6
        [:input.form-control {:type      "text"
                              :value     (:game-id @db)
                              :on-change #(swap! db
                                                 assoc
                                                 :game-id
                                                 (.. % -target -value))}]]]]
     [error-msg]]))