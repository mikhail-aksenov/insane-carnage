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
            [insane-carnage.game :as game]
            [insane-carnage.field :as field]
            [insane-carnage.talk :as talk]
            [medley.core :refer [filter-vals]]))

(enable-console-print!)

;; -------------------------
;; Views

(defn home-page []
  (welcome))

(defn wait-screen []
  (let [game-id (:game-id @db)
        msg (case game-id
              "new" "Creating new game..."
              "random" "Joining random game..."
              (str "Joining game with id " game-id))]
    [:div#wait-screen
     [:h1 msg]
     [:div.throbber-loader "Loading..."]]))

(defn current-page []
  (let [{:keys [state game]} @db]
    ;(println "current-page" state)
    [:div
     [:button {:on-click game/reset} "Reset"]
     [(case state
        :setup welcome
        :waiting wait-screen
        :running field/render-game
        :finished "Game over")]]))

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

(defmulti event-msg-handler
          (fn [type msg] type))

(defmethod event-msg-handler :game/updated
  [_ msg]
  (let [{:keys [game log]} msg]
    ;(println "game/updated" (keys msg))
    (game/update-game! game log)))

(defmethod event-msg-handler :game/joined
  [_ msg]
  (let [{:keys [game unit-id log]} msg]
    (println "game/joined" (:id game) "as" (get-in game [:units unit-id]))
    (game/joined! game unit-id log)))

(defmethod event-msg-handler :default
  [type msg]
  (println "Unhandled msg type" type msg))

(defn event-msg-handler* [{:as ev-msg :keys [id ?data event]}]
  ;(println "Event" event)
  (case (first event)
    :chsk/recv
    (apply event-msg-handler (second event))
    :chsk/state
    (when (-> event second :first-open?)
      (accountant/dispatch-current!))
    (println "Service msg" (first event))))

(defonce router_ (atom nil))
(defn stop-router! [] (when-let [stop-f @router_] (stop-f)))
(defn start-router! []
  (stop-router!)
  (reset! router_
          (sente/start-client-chsk-router!
            talk/ch-chsk event-msg-handler*)))

;; -------------------------
;; Routes

(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
                    (game/reset))

(secretary/defroute "/game/:game-id" [game-id]
                    (when-not (:game @db)
                      (game/join! game-id)))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (start-router!)
  (accountant/configure-navigation!)
  ;(accountant/dispatch-current!)
  (mount-root))
