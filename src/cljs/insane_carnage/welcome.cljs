(ns insane-carnage.welcome
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [insane-carnage.db :refer [db]]))

(defn start-new-game []
  (accountant/navigate! "/game/new"))

(defn join-random-game []
  (accountant/navigate! "/game/random"))

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
        [:a.btn.btn-default {:disabled (or
                                         (not can-start?)
                                         (empty? (:game-id @db)))
                             :href (str "/game/" (:game-id @db))}
         "Join"]]
       [:div.col-xs-2 {:style {:padding "0.5rem 0"}} "game #"]
       [:div.col-xs-6
        [:input.form-control {:type      "text"
                              :value     (:game-id @db)
                              :on-change #(swap! db
                                                 assoc
                                                 :game-id
                                                 (.. % -target -value))}]]]]]))