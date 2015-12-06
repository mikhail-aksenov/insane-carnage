(ns insane-carnage.core
  (:require-macros
    [cljs.core.async.macros :as asyncm :refer (go go-loop)])
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [cljs.core.async :as async :refer (<! >! put! chan)]
            [taoensso.sente :as sente :refer (cb-success?)]
            [insane-carnage.welcome :refer [welcome]]
            [insane-carnage.db :refer [db]]
            [insane-carnage.game :as game]))

(enable-console-print!)

;; -------------------------
;; Views

(defn home-page []
  (welcome))

(defn wait-screen []
  (let [game-id (:game-id @db)
        _ (println "wait-screen" @db)
        msg (case game-id
              "new" "Creating new game..."
              "random" "Joining random game..."
              (str "Joining game with id " game-id))]
    [:div#wait-screen
     [:h1 msg]
     [:div.throbber-loader "Loading..."]]))

(defn current-page []
  (let [{:keys [game-state game]} @db]
    (println "current-page" game-state)
    [:div
     [(case game-state
        :setup welcome
        ;:wait wait-screen
        :run (if game
               game/render-field
               wait-screen))]]))

;; -------------------------
;; Communication

;(let [{:keys [chsk ch-recv send-fn state]}
;      (sente/make-channel-socket! "/chsk"                   ; Note the same path as before
;                                  {:type :auto              ; e/o #{:auto :ajax :ws}
;                                   })]
;  (def chsk chsk)
;  (def ch-chsk ch-recv)                                     ; ChannelSocket's receive channel
;  (def chsk-send! send-fn)                                  ; ChannelSocket's send API fn
;  (def chsk-state state)                                    ; Watchable, read-only atom
;  )

;; -------------------------
;; Routes

(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
                    (game/reset))

(secretary/defroute "/game/:game-id" [game-id]
                    (game/join game-id))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!)
  (accountant/dispatch-current!)
  (mount-root))
