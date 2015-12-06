(ns insane-carnage.repl
  (:use insane-carnage.handler
        ring.server.standalone
        [ring.middleware file-info file]
        [org.httpkit.server :as http-kit :refer [run-server]]
        ;[insane-carnage.engine :as engine]
        [insane-carnage.game :as game]))

(defonce server (atom nil))

(defn get-handler []
  ;; #'app expands to (var app) so that when we reload our code,
  ;; the server is forced to re-resolve the symbol in the var
  ;; rather than having its own copy. When the root binding
  ;; changes, the server picks it up without having to restart.
  (-> #'app
      ; Makes static assets in $PROJECT_DIR/resources/public/ available.
      (wrap-file "resources")
      ; Content-Type, Content-Length, and Last Modified headers for files in body
      (wrap-file-info)))

;(defn start-server
;  "used for starting the server in development mode from REPL"
;  [& [port]]
;  (let [port (if port (Integer/parseInt port) 3000)]
;    (reset! server
;            (serve (get-handler)
;                   {:port port
;                    :auto-reload? true
;                    :join? false}))
;    (println (str "You can view the site at http://localhost:" port))))

;(defn app [req]
;  {:status  200
;   :headers {"Content-Type" "text/html"}
;   :body    "hello HTTP!"})

(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (stop-router!)
    (@server :timeout 100)
    (reset! server nil)))

(defn start-server []
  ;; The #' is useful when you want to hot-reload code
  ;; You may want to take a look: https://github.com/clojure/tools.namespace
  ;; and http://http-kit.org/migration.html#reload
  (start-router!)
  (reset! server (run-server #'app {:port 3000})))

(defn reset []
  (game/reset-all!)
  (stop-server)
  (start-server))

;(defn stop-server []
;  (.stop @server)
;  (reset! server nil))
