(ns insane-carnage.engine
  #?(:clj
           (:require [clojure.core.async :refer [go-loop go]]
                     [clojure.tools.logging :as log])
     :cljs (:require-macros [cljs.core.async.macros :refer [go-loop go]]))
  (:require [#?(:clj  clojure.core.async
                :cljs cljs.core.async) :as async :refer [<! >! chan close! put! to-chan]]
            [#?(:clj  clj-time.core
                :cljs cljs-time.core) :as time]
            [clojure.data :refer [diff]]
            [medley.core :refer [filter-vals remove-vals map-vals drop-upto dissoc-in]]))

;; Definitions

(def tick-duration 100)

(def possible-moves
  #{:none :hit :move-ahead :move-back :turn-left :turn-right})

(def unit-types
  {:swordsman {:hit-damage    15
               :move-duration 4
               :hit-duration  5
               :sight         20
               :hit-cells
                              (fn [{:keys [x y direction]}]
                                (let [local
                                      (case direction
                                        [0 1] [[-1 1] [0 1] [1 1]]
                                        [1 1] [[0 1] [1 1] [1 0]]
                                        [1 0] [[1 1] [1 0] [1 -1]]
                                        [1 -1] [[1 0] [1 -1] [0 -1]]
                                        [0 -1] [[1 -1] [0 -1] [-1 -1]]
                                        [-1 -1] [[0 -1] [-1 -1] [-1 0]]
                                        [-1 0] [[-1 -1] [-1 0] [-1 1]]
                                        [-1 1] [[-1 0] [-1 1] [0 1]])]
                                  (mapv #(mapv + %1 [x y]) local)))}
   :pikeman   {:hit-damage    10
               :move-duration 7
               :hit-duration  10
               :sight         20
               :hit-cells
                              (fn [{:keys [x y direction]}]
                                [direction
                                 (mapv #(* 2) direction)])}
   :bowman    {:hit-damage        20
               :move-duration     3
               :hit-duration      30
               :sight             30
               :hit-move-duration 1
               :hit-cells
                                  (fn [{:keys [x y]}]
                                    [x y])}})

(def possible-directions
  [[0 1]
   [1 1]
   [1 0]
   [1 -1]
   [0 -1]
   [-1 -1]
   [-1 0]
   [-1 1]])

;; --------
;; Common

(defn unit-meta [type]
  (get unit-types type))

(defn- trim-to-bounds [top right bot left [x y :as cell]]
  [(min right (max x left))
   (min bot (max y top))])

(defn- prev-direction [dir]
  (or
    (->> possible-directions
         (take-while #(not= dir %))
         (last))
    (last possible-directions)))

(defn- next-direction [dir]
  (or
    (->> possible-directions
         (drop-upto #(= dir %))
         (first))
    (first possible-directions)))

(defn game-player-ids [{:keys [units]}]
  {:pre [units (map? units)]}
  (->> units
       (filter-vals :player-id)
       (vals)
       (map :player-id)))

(defn dead? [unit]
  (not (pos? (:hp unit))))

;; --------
;; Hits

(defn- new-hit [unit tick]
  (let [meta (get unit-types type)]
    {:moved-at   tick
     :started-at tick
     :x          (:x unit)
     :y          (:y unit)
     :direction  (:direction unit)}))

(defn- can-hit? [unit meta tick]
  (let [hit (:hit unit)]
    (or hit
        (> (- tick (:started-at hit)) (:hit-duration meta)))))

(defn- make-hit [unit meta tick]
  ;(println "make-hit" unit meta tick)
  (if (can-hit? unit meta tick)
    (assoc unit :hit (new-hit unit tick))
    unit))

(defn- cells-bounds [fcell & cells]
  (let [[fx fy] fcell]
    (reduce
      (fn [[top right bot left] [x y]]
        [(min top y)
         (max right x)
         (max bot y)
         (min left x)])
      [fy fx fy fx]
      cells)))

(defn- in-bounds? [bounds cell]
  (let [[top right bot left] bounds
        [x y] cell]
    (and (y >= top)
         (x <= right)
         (y <= bot)
         (x >= left))))

(defn- hit-collides-unit? [hit-cell-set unit]
  (get hit-cell-set [(:x unit) (:y unit)]))

(defn- hit-hurt-victims [hit-cells units]
  (let [collider (partial hit-collides-unit? (set hit-cells))]
    (filter-vals collider units)))

(defn- apply-hit [meta unit]
  (update unit :hp - (:hit-damage meta)))

(defn- process-hit [game offender]
  {:pre [(:units game)]
   :post [(:units %)]}
  (let [tick (:tick game)
        hit (:hit offender)
        units (:units game)
        meta (get unit-types (:type offender))
        over? (> (- tick (:moved-at hit)) (:hit-duration meta))]
    (if over?
      (dissoc-in game [:units (:id offender) :hit])
      (let [other-units (dissoc units (:id offender))
            hit-cells ((:hit-cells meta) hit)
            victim-ids (keys (hit-hurt-victims hit-cells other-units))
            apply-hit (partial apply-hit meta)
            next-units
            (reduce
              (fn [units victim-id]
                (update units victim-id apply-hit))
              units
              victim-ids)]
        (assoc game :units next-units)))))

(defn- process-all-hits [game]
  {:pre [(:units game)]
   :post [(:units %)]}
  (reduce
    (fn [game [_ unit]]
      (if (:hit unit)
        (process-hit game unit)
        game))
    game
    (:units game)))

;; --------
;; Move

(defn process-moveable [moveable move-duration game]
  (let [{:keys [x y direction move moved-at]} moveable
        {:keys [width height tick]} game]
    (if-not (>= (- tick moved-at) move-duration)
      moveable
      (cond
        (get #{:move-ahead :move-back} move)
        (let [shifted (if (= move :move-ahead)
                        (map + [x y] direction)
                        (map - [x y] direction))
              [nx ny] (trim-to-bounds 0 width height 0 shifted)]
          (assoc moveable
            :x nx
            :y ny
            :moved-at tick))
        (get #{:turn-left :turn-right} move)
        (let [new-dir (if (= move :turn-right)
                        (prev-direction direction)
                        (next-direction direction))]
          (assoc moveable
            :direction new-dir
            :moved-at tick))
        :else moveable))))

(defmulti process-hit-move
          (fn [unit meta game] (:type unit)))

(defmethod process-hit-move :default [unit {:keys [hit-move-duration]} game]
  (if (pos? hit-move-duration)
    (update unit :hit process-moveable hit-move-duration game)
    unit))

(defn- process-unit-move [unit game]
  {:pre [(:id unit)]
   :post [(:id unit)]}
  (if (dead? unit)
    unit
    (let [meta (get unit-types (:type unit))
          move (:move unit)
          moved-unit
          (cond
            (= :hit move) (make-hit unit meta (:tick game))
            (not= :none move) (process-moveable unit (:move-duration meta) game)
            :else unit)]
      (if-not (:hit unit)
        moved-unit
        (update moved-unit :hit process-hit-move meta game)))))

(defn- process-all-moves [{:keys [units] :as game}]
  (let [next-units (->> units
                        (map-vals
                          #(process-unit-move % game)))]
    (assoc game :units next-units)))

;; --------
;; Game state

(defn- all-dead? [units]
  (every?
    (fn [[_ unit]]
      (not (pos? (:hp unit))))
    units))

(defn- no-live-players? [units]
  (every?
    (fn [[_ unit]]
      (not (:player-id unit)))
    units))

(defn- update-state [game]
  {:pre [(:units game)]
   :post [(:units %)]}
  (let [units (:units game)
        all-dead (all-dead? units)
        no-live-players (no-live-players? units)
        game-over? (or all-dead no-live-players)]
    (cond-> game
            game-over?
            (assoc :state :finished
                   :finished-at (time/now)))))

;; --------
;; Api

(defn log-live-units [game]
  (->> (:units game)
       (vals)
       ;(filter :player-id)
       (filter #(= :move-back (:move %))) (take 1)
       (map
         #(println "Player" (select-keys % [:player-id :x :y :move :direction :type])))
       (doall))
  game)

(defn process-tick [game tick]
  {:pre [(> tick (:tick game))]}
  (let [next-game (-> game
                      (assoc :tick tick)
                      ;(process-ai)
                      (process-all-moves)
                      (process-all-hits)
                      (update-state)
                      ;(log-live-units)
                      )]
    next-game))

(defn set-unit-move [game unit-id move]
  {:pre [(get possible-moves move)]}
  (assoc-in game [:units unit-id :move] move))