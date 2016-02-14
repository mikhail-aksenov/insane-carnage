(ns insane-carnage.handler
  (:require [compojure.core :refer [GET POST defroutes]]
            [compojure.route :refer [not-found resources]]
            [clojure.core.async :refer [go-loop go <! >! chan close! put! to-chan]]
            [ring.middleware.defaults :refer [site-defaults wrap-defaults]]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [include-js include-css]]
            [prone.middleware :refer [wrap-exceptions]]
            [ring.middleware.reload :refer [wrap-reload]]
            [environ.core :refer [env]]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit :refer [sente-web-server-adapter]]
            [clojure.tools.logging :as log]
            [chime :refer [chime-ch]]
            [clj-time.core :as time]
            [insane-carnage.util :as util]
            [insane-carnage.game :as game]
            [medley.core :refer :all]
            [insane-carnage.engine :as engine]))

(def mount-target
  [:div#app
   [:h3 "ClojureScript has not been compiled!"]
   [:p "please run "
    [:b "lein figwheel"]
    " in order to start the compiler"]])

(def loading-page
  (html
    [:html
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name    "viewport"
              :content "width=device-width, initial-scale=1"}]
      (include-css (if (env :dev) "/css/site.css" "css/site.min.css"))
      (include-css "/css/bootstrap.css")
      (include-css "http://css-spinners.com/css/spinner/throbber.css")]
     [:body
      mount-target
      (include-js "/js/app.js")]]))

(declare chsk-send!)

(declare event-msg-handler*)

(declare game-event-handler*)

;; -------------------------
;; Lifecycle

(defn- get-player-id [ring-req]
  (get-in ring-req [:cookies "player-id" :value]))

(let [{:keys [ch-recv send-fn ajax-post-fn ajax-get-or-ws-handshake-fn
              connected-uids]}
      (sente/make-channel-socket-server!
        sente-web-server-adapter
        {:packer     :edn
         :user-id-fn get-player-id})]
  (def ring-ajax-post ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk ch-recv)                                     ; ChannelSocket's receive channel
  (def chsk-send! send-fn)                                  ; ChannelSocket's send API fn
  (def connected-uids connected-uids)                       ; Watchable, read-only atom
  )

(defonce router_ (atom nil))

(defn stop-router! []
  (when-let [stop-f @router_]
    (log/info "Stop Sente")
    (stop-f)))

(defn start-router! []
  (stop-router!)
  (log/info "Start Sente")
  (reset! router_
          (sente/start-server-chsk-router!
            ch-chsk event-msg-handler*)))

(defonce game-server (atom nil))

(defn stop-game-server! []
  (when-let [[ch-in ch-out] @game-server]
    (log/info "Stop Game Server")
    (close! ch-in)
    (close! ch-out)))

(defn start-game-server! []
  (stop-game-server!)
  (log/info "Start Game Server")
  (reset! game-server (game/new-server))
  (let [[ch-in ch-out] @game-server]
    (go-loop []
      (when-let [msg (<! ch-out)]
        (game-event-handler* msg)
        (recur)))))

(defn stop-app! []
  (stop-router!)
  (stop-game-server!))

(defn start-app! []
  (start-router!)
  (start-game-server!))

;; -------------------------
;; Game Handlers

(defn prepare-game-for-client [game]
  (dissoc game :ch-in :ch-out))

(defmulti game-event-handler :type)

(defn game-event-handler* [msg]
  (log/debugf "Game event: %s" msg)
  (game-event-handler msg))

(defmethod game-event-handler :game/joined
  [{:keys [game player-id unit log] :as msg}]
  (log/info "Game Event" (dissoc msg :game) (:id unit) (type player-id))
  (chsk-send! player-id [:game/joined {:game    (prepare-game-for-client game)
                                       :unit-id (:id unit)
                                       :log log}]))

(defmethod game-event-handler :game/updated
  [{:keys [game] :as msg}]
  {:pre [(-> game engine/game-player-ids empty? not)]}
  (log/debug "Game Event" (dissoc msg :game) (:id game))
  (let [ids (engine/game-player-ids game)
        prepared-game (prepare-game-for-client game)]
    (->> ids
         (map
           (fn [id]
             (chsk-send! id [:game/updated {:game prepared-game}]))
           )
         (doall))))

;; -------------------------
;; WebSocket Handlers

(defmulti event-msg-handler :id)

(defn event-msg-handler* [{:as ev-msg :keys [id ?data event]}]
  (log/debug "Event: %s" event)
  (event-msg-handler ev-msg))

(defmethod event-msg-handler :game/join
  [{:as ev-msg :keys [event id ring-req ?reply-fn]}]
  (let [[_ {:keys [player-name game-id]}] event
        [ch-in] @game-server
        player-id (get-player-id ring-req)]
    (log/infof "%s (%s) joins game %s" player-name player-id game-id)
    (case game-id
      "new" (put! ch-in {:type        :game/new
                         :game-id     (game/uuid)
                         :player-id   player-id
                         :player-name player-name})
      "random" (put! ch-in {:type        :game/join-random
                            :player-id   player-id
                            :player-name player-name})
      (put! ch-in {:type        :game/join
                   :game-id     :game-id
                   :player-id   player-id
                   :player-name player-name}))))

(defmethod event-msg-handler :game/leave
  [{:as ev-msg :keys [event id ring-req ?reply-fn]}]
  (let [player-id (get-player-id ring-req)
        [ch-in] @game-server]
    (log/infof "%s leaves game" player-id)
    (put! ch-in {:type      :game/leave
                 :player-id player-id})))

(defmethod event-msg-handler :player/move
  [{:as ev-msg :keys [event id ring-req ?reply-fn]}]
  (let [player-id (get-player-id ring-req)
        [ch-in] @game-server
        [_ {:keys [move]}] event]
    (log/infof "Player %s moves %s" player-id move)
    (put! ch-in {:type      :player/move
                 :move      move
                 :player-id player-id})))

(defmethod event-msg-handler :default                       ; Fallback
  [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (let [session (:session ring-req)
        uid (:uid session)]
    (when-not (.startsWith (-> event first str) ":chsk")
      (log/error "Unhandled event: %s" event))
    (when ?reply-fn
      (?reply-fn {:umatched-event-as-echoed-from-server event}))))

;; -------------------------
;; Routes

(defroutes routes
           (GET "/about" [] loading-page)
           (GET "/chsk" req (ring-ajax-get-or-ws-handshake req))
           (POST "/chsk" req (ring-ajax-post req))
           (resources "/")
           ;(Get "/game/:id" [id]
           ;     (game/join-or-create-game ))
           (GET "/*" [] loading-page)
           (not-found "Not Found"))

(def app
  (let [handler (wrap-defaults #'routes site-defaults)]
    (if (env :dev) (-> handler wrap-exceptions wrap-reload) handler)))

