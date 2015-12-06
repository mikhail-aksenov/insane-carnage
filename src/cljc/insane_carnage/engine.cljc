(ns insane-carnage.engine
  #?(:clj (:require [clojure.core.async :refer [go-loop go]])
     :cljs (:require-macros [cljs.core.async.macros :refer [go-loop go]]))
  (:require [#?(:clj  clojure.core.async
                :cljs cljs.core.async) :as async :refer [<! >! chan close! put! to-chan]]
            [medley.core :refer [map-vals]]))

(defonce input-channel (chan))

(def game-channels (atom {}))
(def games (atom {}))
(def loop-msecs 10000)

(defn register-game [game-id]
  (when-not (@game-channels game-id)
    (swap! games assoc game-id { :tick 0 :status :in-progress})
    (swap! game-channels assoc game-id { :in (chan) :out (chan) })))

(defn unregister-game [game-id]
  (do
    (when-let [chans (@game-channels game-id)]
      (async/close! (:in chans))
      (async/close! (:out chans)))
    (swap! game-channels dissoc game-id)))

(defn start-engine []
  (go
    (while true
        (let [message (<! input-channel)
           {:keys [game-id msg]} message]
        (when-let [ch (get-in @game-channels [game-id :in])]
          (>! ch msg))))))

(defn consume-out [game-id]
  (when-let [ch (get-in @game-channels [game-id :out])]
    (go-loop []
             (when-let [v (<! ch)]
               (println v System/currentTimeMillis.)
               (recur)))))

(defn game-in-progress? [game-id]
  (= (get-in @games [game-id :status]) :in-progress))

(defn game-loop [game-id]
  (let [in-events (get-in @game-channels [game-id :in])
        out-events (get-in @game-channels [game-id :out])]
    (go
      (while (game-in-progress? game-id)
        (let [msg (<! in-events)]
          (when (= (:tick msg) (get-in @games [game-id :tick]))
            (println "Player action")))))
    (go
      (while (game-in-progress? game-id)
        (<! (async/timeout loop-msecs))
        (swap! games update-in [game-id :tick] inc)
        (>! out-events { :game-id game-id :msg (get-in @games [game-id :tick])})))))

;(register-game 1)
;(start-engine)
;(consume-out 1)
;(game-loop 1)