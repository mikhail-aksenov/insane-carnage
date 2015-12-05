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
            [insane-carnage.db :refer [db]]))

;; -------------------------
;; Views

(defn home-page []
  (welcome))

(defn about-page []
  [:div [:h2 "About insane-carnage"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

(defn wait-screen []
  (let [game-id (:game-id @db)
        msg (case game-id
              "new" "Creating new game..."
              "random" "Joining random game..."
              (str "Joining game with id " game-id))]
    [:div#wait-screen
     [:h1 msg]
     [:div.throbber-loader "Loading..."]]))

;; -------------------------
;; Communication

(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! "/chsk"                   ; Note the same path as before
                                  {:type :auto              ; e/o #{:auto :ajax :ws}
                                   })]
  (def chsk chsk)
  (def ch-chsk ch-recv)                                     ; ChannelSocket's receive channel
  (def chsk-send! send-fn)                                  ; ChannelSocket's send API fn
  (def chsk-state state)                                    ; Watchable, read-only atom
  )

;; -------------------------
;; Handlers

(defn join-game [game-id]
  (.log js/console "Join game" game-id)
  (session/put! :current-page #'wait-screen)
  (chsk-send! [:game/start {:player-name (:player-name @db)
                            :player-id (:player-id @db)
                            :game-id game-id}]
              5000
              (fn [reply]
                (js/console.log "Reply received" reply)))
  (swap! db assoc :game-id game-id))

;; -------------------------
;; Routes

(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
                    (session/put! :current-page #'welcome))

(secretary/defroute "/game/:game-id" [game-id]
                    (join-game game-id))

(secretary/defroute "/about" []
                    (session/put! :current-page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!)
  (accountant/dispatch-current!)
  (mount-root))
