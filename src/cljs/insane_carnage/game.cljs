(ns insane-carnage.game
  (:require [reagent.session :as session]
            [accountant.core :as accountant]
            [insane-carnage.db :refer [db]]
            [insane-carnage.talk :refer [chsk-send!]]))

(defn find-your-game-player [game]
  (let [player-id (:player-id @db)]
    (->> (:players game)
         (vals)
         (filter #(= player-id (:player-id %)))
         (first)
         (:id))))

(defn- joined [game]
  (js/console.log "Joined game" (clj->js game))
  (if (= game :chsk/timeout)
    (swap! db
           #(-> %
                (assoc :game-state :setup)
                (assoc :error "Something went wrong.")))
    (swap! db
           #(-> %
                (assoc :game-id (:game-id game))
                (assoc :game game)
                (assoc :game-player-id (find-your-game-player game))))))

(defn join [game-id]
  (.log js/console "Join game" game-id)
  (chsk-send! [:game/start {:player-name (:player-name @db)
                            :player-id   (:player-id @db)
                            :game-id     game-id}]
              10000
              joined)
  (swap! db
         #(-> %
              (assoc :game-state :run)
              (assoc :game-id game-id))))

(defn reset []
  (swap! db
         #(-> %
              (assoc :game-state :setup)
              (dissoc :game-id)
              (dissoc :game))))

(defn quit-game []
  (println "Quit game")
  (chsk-send! [:game/quit])
  (reset))

;; ----------
;; Rendering

(def tile-size 20)

(defn to-view [x y]
  [(* tile-size x) (* tile-size y)])

(defn get-visible-bounds [{:keys [width height sight]} player]
  (let [{:keys [x y]} player]
    {:left   (max 0 (- x sight))
     :top    (max 0 (- y sight))
     :right  (min width (+ x sight))
     :bottom (min height (+ y sight))}))

(defn render-tiles [{:keys [width height sight] :as game}
                    visible-bounds]
  (for [w (range (:left visible-bounds) (:right visible-bounds))
        h (range (:top visible-bounds) (:bottom visible-bounds))
        :let [x (* w tile-size)
              y (* h tile-size)]]
    [:rect {:key    (str w "-" h)
            :x      x
            :y      y
            :width  tile-size
            :height tile-size}]))

(defn sight-circle [sight]
  (let [r (* tile-size sight)]
    [:circle {:class "sight-circle"
              :cx    r
              :cy    r
              :r     r
              :mask  "url(#sight-grad"}]))

(defn render-hud [game player]
  [:div#game-hud
   [:div.player-state
    [:div (str "HP" (:hp player))]]
   [:div.game-menu
    [:button.btn.btn-default
     {:on-click quit-game}
     "Quit"]]])

(defn- seen? [{:keys [left right top bottom] :as visible-bounds} x y]
  (and (>= x left)
       (<= x right)
       (>= y top)
       (<= y bottom)))

(defn render-players [players visible-bounds]
  (for [[id player] players
        :let [{:keys [x y]} player
              [w h] (to-view x y)
              hs (/ tile-size 2)]
        :when (seen? visible-bounds x y)]
    [:circle {:key   id
              :cx    (+ w hs)
              :cy    (+ h hs)
              :r     hs
              :class "player"}]))

(defn top-transform [{:keys [x y] :as player} sight]
  (let [w (* tile-size x)
        h (* tile-size y)
        r (* tile-size sight)]
    (str "translate("
         (- r w)
         " "
         (- r h)
         ")")))

(defn render-bg-rect [sight]
  [:rect.bg {:x      0
             :y      0
             :width  (* sight tile-size 2)
             :height (* sight tile-size 2)}])

(defn render-field []
  (let [{:keys [game player-id game-player-id]} @db
        {:keys [sight players]} game
        player (get-in game [:players game-player-id])
        visible-bounds (get-visible-bounds game player)]
    (println "render-field player" game-player-id player)
    [:div#game-field
     [:svg {:width  600
            :height 600}
      [:defs                                                ;{:dangerouslySetInnerHTML {:__html (svg-defs sight)}}
       [:radialGradient#sight-grad {:cx "50%"
                                    :cy "50%"
                                    :r  "50%"
                                    :fx "50%"
                                    :fy "50%"}
        [:stop {:offset 0.75
                :style  {:stop-color   "white"
                         :stop-opacity "1"}}]
        [:stop {:offset 1
                :style  {:stop-color   "black"
                         :stop-opacity "0"}}]]
       [:mask#sight-mask
        [:circle {:cx   (* (:x player) tile-size)
                  :cy   (* (:y player) tile-size)
                  :r    (* sight tile-size)
                  :fill "url(#sight-grad)"}]]]
      (render-bg-rect sight)
      [:g {:transform (top-transform player sight)
           :style     {:mask "url(#sight-mask)"}}
       [:g.tile-group
        (render-tiles game visible-bounds)]
       [:g.player-group
        (render-players players visible-bounds)]]]
     (render-hud game player)
     ]))