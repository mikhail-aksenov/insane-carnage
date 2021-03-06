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
            [insane-carnage.util :as util]
            [insane-carnage.game :as game]))

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

;; -------------------------
;; WebSoket Handlers

(declare chsk-send!)

(defn send-game-updates [game-id update player-id]
  (chsk-send! player-id [:game/update {:game-id game-id
                                       :update  update}]))

(defn listen-game-updates []
  (go-loop []
    (when-let [{:keys [game-id update] :as msg} (<! game/update-chan)]
      (->> (game/game-live-player-ids game-id)
           (map #(send-game-updates game-id update %))))))

(declare event-msg-handler*)

(defonce router_ (atom nil))
(defn stop-router! [] (when-let [stop-f @router_] (stop-f)))
(defn start-router! []
  (stop-router!)
  (log/info "Start Sente router")
  (let [{:keys [ch-recv send-fn ajax-post-fn ajax-get-or-ws-handshake-fn
                connected-uids]}
        (sente/make-channel-socket! sente-web-server-adapter {})]
    (def ring-ajax-post ajax-post-fn)
    (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
    (def ch-chsk ch-recv)                                   ; ChannelSocket's receive channel
    (def chsk-send! send-fn)                                ; ChannelSocket's send API fn
    (def connected-uids connected-uids)                     ; Watchable, read-only atom
    (reset! router_ (sente/start-chsk-router! ch-chsk event-msg-handler*))
    (listen-game-updates)))

(defmulti event-msg-handler :id)

(defn event-msg-handler* [{:as ev-msg :keys [id ?data event]}]
  (log/debug "Event: %s" event)
  (event-msg-handler ev-msg))

(defn- get-player-id [ring-req]
  (get-in ring-req [:cookies "player-id" :value]))

(defmethod event-msg-handler :game/start
  [{:as ev-msg :keys [event id ring-req ?reply-fn]}]
  (let [[_ {:keys [player-name game-id]}] event
        player-id (get-player-id ring-req)]
    (log/info "Join game" game-id "as" player-name "(" player-id ")")
    (if-let [game
             (case game-id
               "new" (game/start-new-game player-id player-name)
               "random" (game/join-random-game player-id player-name)
               (game/join-or-create-game player-id player-name game-id))]
      (?reply-fn game))))

(defmethod event-msg-handler :game/quit
  [{:as ev-msg :keys [event id ring-req ?reply-fn]}]
  (let [player-id (get-player-id ring-req)]
    (game/unbind-player-from-game player-id)))

(defmethod event-msg-handler :player/action
  [{:as ev-msg :keys [event id ring-req ?reply-fn]}]
  (let [player-id (get-player-id ring-req)
        [_ action] event]
    (log/info "Player" player-id "action" action)
    (game/process-action player-id action)))

(defmethod event-msg-handler :default                       ; Fallback
  [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (let [session (:session ring-req)
        uid (:uid session)]
    (when-not (.startsWith (-> event first str) ":chsk")
      (log/error "Unhandled event: %s" event))
    (when ?reply-fn
      (?reply-fn {:umatched-event-as-echoed-from-from-server event}))))

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
