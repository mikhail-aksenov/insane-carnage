(ns insane-carnage.server
  (:use org.httpkit.server)
  (:require [insane-carnage.handler :refer [app start-router! stop-router!]]
            [environ.core :refer [env]]
            ;[ring.adapter.jetty :refer [run-jetty]]
            [org.httpkit.server :as http-kit :refer [run-server]]
            [taoensso.sente.server-adapters.http-kit :refer [sente-web-server-adapter]])
  (:gen-class))

 ;(defn -main [& args]
 ;  (let [port (Integer/parseInt (or (env :port) "3000"))]
 ;    (run-jetty app {:port port :join? false})))

;(defn app [req]
;  {:status  200
;   :headers {"Content-Type" "text/html"}
;   :body    "hello HTTP!"})

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 100)
    (stop-router!)
    (reset! server nil)))

(defn -main [&args]
  ;; The #' is useful when you want to hot-reload code
  ;; You may want to take a look: https://github.com/clojure/tools.namespace
  ;; and http://http-kit.org/migration.html#reload
  (start-router!)
  (reset! server (run-server #'app {:port 3000})))