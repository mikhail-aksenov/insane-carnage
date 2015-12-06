(ns insane-carnage.engine
  #?(:clj
           (:require [clojure.core.async :refer [go-loop go]])
     :cljs (:require-macros [cljs.core.async.macros :refer [go-loop go]]))
  (:require [#?(:clj  clojure.core.async
                :cljs cljs.core.async) :as async :refer [<! >! chan close! put! to-chan]]
            [#?(:clj  clj-time.core
                :cljs cljs-time.core) :as time]))

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

(defn current-time-millis []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

(defn consume-out [game-id]
  (when-let [ch (get-in @game-channels [game-id :out])]
    (go-loop []
             (when-let [v (<! ch)]
               (println v (current-time-millis))
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

(defn neighbor-direction [direction offset]
  (let [idx (index-of direction cw-directions)
        nidx (+ idx offset)
        cnt (count cw-directions)]
    (cond
      (> nidx cnt) (- nidx cnt)
      (< nidx 0) (+ nidx cnt)
      :else nidx)))

(defn update-player-direction [game player turn]
  (let [{:keys [direction]} player
        new-dir (case turn
                  :left (neighbor-direction direction -1)
                  :right (neighbor-direction direction 1))]
    (update-in game [:players (:player player) direction] new-dir)))

(defn update-move-state [game player stop?]
  (assoc-in game
            [:players (:player-id player)]
            (if stop? :staying :moving)))

(defn move-player [game player]
  (let [{:keys [width height]} game
        {:keys [x y direction player-id]} player
        [nx ny] (map + [x y] direction)
        [nx ny] [(keep-in-range nx 0 width)
                 (keep-in-range ny 0 height)]]
    (update-in game
               [:players player-id]
               assoc :x nx :y ny)))

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