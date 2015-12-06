(ns insane-carnage.engine
  #?(:clj
           (:require [clojure.core.async :refer [go-loop go]]
                     [clojure.tools.logging :as log])
     :cljs (:require-macros [cljs.core.async.macros :refer [go-loop go]]))
  (:require [#?(:clj  clojure.core.async
                :cljs cljs.core.async) :as async :refer [<! >! chan close! put! to-chan]]
            [#?(:clj  clj-time.core
                :cljs cljs-time.core) :as time]
            [clojure.data :refer [diff]]))

;; Game map structure
;; { :game-id guid
;;   :players {
;;     :player-id {
;;       :position {
;;       }
;;       :direction _
;;     :health
;;     }
;;   }
;;   :attacked-fields {
;;     :player-id {
;;       :position {
;;         :x _
;;         :y _
;;       }
;;
;;
;;     }
;;   }
;;   :status :finished OR :in-progress
;; }
;;

(declare process-all-moves)

(defonce input-channel (chan))

(def game-channels (atom {}))
(def games (atom {}))
(def loop-msecs 10000)

(defn register-game [game]
  (let [game-id (:game-id game)]
    (when-not (@game-channels game-id)
      (swap! games assoc game-id game)
      (swap! game-channels assoc game-id {:in (chan) :out (chan)}))))

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

(defn current-time-millis []
  #?(:clj  (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

(defn consume-out [game-id]
  (when-let [ch (get-in @game-channels [game-id :out])]
    (go-loop []
             (when-let [v (<! ch)]
               (println v (current-time-millis))
               (recur)))))

(defn game-in-progress? [game-id]
  (= (get-in @games [game-id :status]) :in-progress))

(defn update-game [game-id]
  (let [old-game (get @games game-id)
        new-game (process-all-moves game-id)
        [_ upd-chunk _] (diff old-game new-game)
        out-events (get-in @game-channels [game-id :out])]
    (put! out-events {:game-id game-id
                      :update upd-chunk})))

(defn game-loop [game-id]
  (let [in-events (get-in @game-channels [game-id :in])
        out-events (get-in @game-channels [game-id :out])]
    (go
      (while (game-in-progress? game-id)
        (let [msg (<! in-events)]
          (when (= (:tick msg) (get-in @games [game-id :tick]))
            ;(println "Player action")
            (update-game game-id)))))
    (go
      (while (game-in-progress? game-id)
        (<! (async/timeout loop-msecs))
        (swap! games update-in [game-id :tick] inc)
        (>! out-events {:game-id game-id :msg (get-in @games [game-id :tick])})))))

;(register-game 1)
(start-engine)
;(consume-out 1)
;(game-loop 1)

;; ----------------
;; Move

(defn- keep-in-range [n min max]
  (max min (min max n)))

(def cw-directions [[0 1]
                    [1 1]
                    [1 0]
                    [1 -1]
                    [0 -1]
                    [-1 -1]
                    [-1 0]
                    [-1 1]])

(defn- index-of [x coll]
  (->> coll
       (keep-indexed
         (fn [idx v]
           (when (= v x) idx)))
       (first)))

(defn log-info [& args]
  #?(:clj  (log/info args)
     :cljs (apply println args)))

(defn neighbor-direction [direction offset]
  (log-info "neighbor-direction" direction offset cw-directions)
  (let [idx (index-of direction cw-directions)
        cnt (count cw-directions)
        nidx (+ idx offset)
        nidx (cond
               (> nidx cnt) (- nidx cnt)
               (< nidx 0) (+ nidx cnt)
               :else nidx)]
    (get cw-directions nidx)))

(defn turn-player [game game-player turn-direction]
  (let [{:keys [direction]} game-player
        new-dir (case turn-direction
                  :left (neighbor-direction direction -1)
                  :right (neighbor-direction direction 1))]
    (assoc-in game [:players (:id game-player) :direction] new-dir)))

(defn move-player [game game-player direction]
  (update-in game
             [:players (:id game-player)]
             assoc
             :state :moving
             :move-direction (case direction
                               :forward 1
                               :backward -1)))

(defn stop-player [game game-player]
  (assoc-in game
            [:players (:id game-player)]
            :staying))

(defn process-player-move [game game-player]
  (let [{:keys [width height]} game
        {:keys [x y direction move-direction id]} game-player
        [nx ny] (->> direction
                     (map #(* % move-direction))
                     (map + [x y]))
        [nx ny] [(keep-in-range nx 0 width)
                 (keep-in-range ny 0 height)]]
    (update-in game
               [:players id]
               assoc
               :x nx
               :y ny)))

(defn process-all-moves [game-id]
  (let [game (get @games game-id)]
    (reduce
      process-player-move
      game
      (->> game :players vals))))

;; ----------------
;; Hit

(defn interval-in-ms [time1 time2]
  (- time2 time1))

(def hits-meta
  {:swordsman {:damage 40 :duration 125}
   :pikemane  {:damage 30 :duration 125}})

(defn player-alive? [player]
  (pos? (:hp player)))

(defmulti get-hit-shape :type)

(defmethod get-hit-shape :swordsman [player]
  (let [{:keys [x y direction]} player
        side-hits (case direction
                    [0 1] [[-1 1] [1 1]]
                    [1 1] [[0 1] [1 0]]
                    [1 0] [[1 1] [1 -1]]
                    [1 -1] [[1 0] [0 -1]]
                    [0 -1] [[1 -1] [-1 -1]]
                    [-1 -1] [[0 -1] [-1 0]]
                    [-1 0] [[-1 -1] [-1 1]]
                    [-1 1] [[-1 0] [0 1]])
        hits (conj side-hits direction)
        abs-hits (for [[xx yy] hits]
                   [(+ x xx) (+ y yy)])]
    abs-hits))

(defmethod get-hit-shape :pikeman [player]
  (let [{:keys [x y direction]} player
        hits [direction
              (mapv #(* 2 %) direction)]
        abs-hits (for [[xx yy] hits]
                   [(+ x xx) (+ y yy)])]
    abs-hits))

(defn player-at-hit? [hit-shape {:keys [x y] :as player}]
  (hit-shape [x y]))

(defn players-at-hit [players hit-shape]
  (->> players
       (vals)
       (filter #(and (player-alive? %)
                     (player-at-hit? hit-shape %)))))

(defn apply-hit-all-affected [game players damage]
  (reduce
    (fn [g player]
      (update-in g
                 [:players (:player-id player) :hp]
                 #(max 0 (- % damage))))
    game
    players))

(defn update-hit-time [game player ts]
  (assoc-in game [:players (:player-id player) :last-hit-at] ts))

(defn can-hit? [player ts {:keys [duration]}]
  (let [interval (interval-in-ms (:last-hit-at player) ts)]
    (> interval duration)))

(defn process-hit [game player ts]
  (let [{:keys [damage] :as hit-meta} (get hits-meta (:type player))]
    (when (can-hit? player ts hit-meta)
      (let [{:keys [players]} game
            hit-shape (set (get-hit-shape player))
            affected-players (players-at-hit players hit-shape)]
        (-> game
            (apply-hit-all-affected affected-players damage)
            (update-hit-time player ts))))))

;; --------
;; Actions

(defn get-game-player [game {:keys [game-player-id] :as player}]
  (get-in game [:players game-player-id]))

(defmulti process-action
          (fn [game player action] (:type action)))

(defmethod process-action :hit [game player action]
  (let [ts (current-time-millis)]
    (process-hit game player ts)))

(defmethod process-action :turn [game player action]
  (turn-player game (get-game-player game player) (:direction action)))

(defmethod process-action :move [game player action]
  (move-player game (get-game-player game player) (:direction action)))

(defmethod process-action :stop [game player action]
  (stop-player game (get-game-player game player)))