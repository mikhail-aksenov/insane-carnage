(ns insane-carnage.engine
  #?(:clj
           (:require [clojure.core.async :refer [go-loop]])
     :cljs (:require-macros [cljs.core.async.macros :refer [go-loop]]))
  (:require [#?(:clj  clojure.core.async
                :cljs cljs.core.async) :as async :refer [<! chan close! put! to-chan]]
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

(def turn-msecs 125)

(defonce games (atom {}))

(defonce direction-map {:left       [-1 0]
                        :up-left    [-1 -1]
                        :up         [0 -1]
                        :up-right   [1 -1]
                        :right      [1 0]
                        :down-right [1 1]
                        :down       [0 1]
                        :down-left  [-1 1]})

(defn new-uuid
  []
  #?(:clj  (java.util.UUID/randomUUID)
     :cljs (random-uuid)))

(defn rand-range [min max]
  (+ min (rand-int max)))

(defn test-player []
  {(new-uuid) {
               :position {:x (rand-range 0 2)
                          :y (rand-range 0 2)}}})

(defn make-game [x-size y-size]
  {:game-id         (new-uuid)
   :status          :in-progress
   :players         {}
   :attacked-fields {}
   :x-size          x-size
   :y-size          y-size})

(defn ->positions
  [players]
  (reduce-kv (fn [coll _ v]
               (conj coll (:position v))) [] players))

(defn random-position [min max prohibited]
  (loop []
    (let [pos {:x (rand-range min max)
               :y (rand-range min max)}]
      (if (some prohibited pos)
        (recur)
        pos))))

;; (apply merge (take 5 (repeatedly test-player)))

;(defn make-player [player-id players attacked-fields]
;  (let [prohibited-positions (into []
;                                   (concat (->positions players)
;                                           (->positions attacked-fields)))
;        position 1]
;    {player-id {:position {:x :y}
;
;                }}))

(defn player-move [old-pos move]
  (let [[dx dy] (direction-map move)
        [x y] old-pos]
    [(+ x dx) (+ y dy)]))

(defn make-move [state player]
  (let [{:keys [player-id move]} player
        {:keys [direction hit]} move]))

;(defn game-loop [state event-ch]
;  (go-loop []
;           (when (= (:status state) :in-progress)
;             (let [[v p]]
;               (async/alts! [event-ch (async/timeout turn-msecs)])
;                          ())
;
;
;             (recur))))

;; ----------------
;; Hit collision

(defmulti player-hit?
          (fn [player hit] (:type hit)))

(defmethod player-hit? :sword [player hit]
  )

(defn- player-under-hit [all-players hit]
  (let [{:keys [player-id]} hit
        players (dissoc all-players player-id)]
    (->> players
         (vals)
         (map #(player-hit? % hit))
         (remove nil)
         (first))))

(defn find-hit-collisions [game]
  (let [{:keys [players hits]} game
        hit-collision (partial player-under-hit players)]
    (->> hits
         (map hit-collision))))

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

