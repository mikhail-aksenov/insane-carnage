(ns insane-carnage.engine
  #?(:clj (:require [clojure.core.async :refer [go-loop]])
     :cljs (:require-macros [cljs.core.async.macros :refer [go-loop]]))
  (:require [#?(:clj  clojure.core.async
                :cljs cljs.core.async) :as async :refer [<! chan close! put! to-chan]]
            [medley.core :refer [map-vals]]))

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

(defonce direction-map { :left [-1 0]
                         :up-left [-1 -1]
                         :up [0 -1]
                         :up-right [1 -1]
                         :right [1 0]
                         :down-right [1 1]
                         :down [0 1]
                         :down-left [-1 1]})

(defn new-uuid
  []
  #?(:clj (java.util.UUID/randomUUID)
     :cljs (random-uuid)))

(defn rand-range [min max]
  (+ min (rand-int max)))

(defn test-player []
  { (new-uuid) {
     :position { :x (rand-range 0 2)
                 :y (rand-range 0 2)}}})

(defn make-game [x-size y-size]
  { :game-id (new-uuid)
    :status :in-progress
    :players {}
    :attacked-fields {}
    :x-size x-size
    :y-size y-size })

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

(defn make-player [player-id players attacked-fields]
  (let [prohibited-positions (into []
                                   (concat (->positions players)
                                           (->positions attacked-fields)))
        position 0]
  { player-id { :position { :x :y }

               }}))

(defn player-move [old-pos move]
  (let [[dx dy] (direction-map move)
        [x y] old-pos]
    [(+ x dx) (+ y dy)]))

(defn make-move [state player]
  (let [{:keys [player-id move]} player
        {:keys [direction hit]} move]))

(defn game-loop [state event-ch]
  (go-loop []
    (when (= (:status state) :in-progress)
      (let [[v p]] (async/alts! [event-ch (async/timeout turn-msecs)])
        ())


      (recur))))
