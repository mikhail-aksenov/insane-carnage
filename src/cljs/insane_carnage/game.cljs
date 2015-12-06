(ns insane-carnage.game
  (:require [reagent.session :as session]
            [reagent.core :as reagent :refer [atom]]
            [accountant.core :as accountant]
            [clojure.string :as str]
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
  (accountant/navigate! "/"))

(defn key-code->action [code]
  (if (= code 32)
    {:state :hit}
    (if-let [dir (case code
                   37 :left                                 ; arrow left
                   65 :left                                 ; a
                   38 :forward                              ; up arrow
                   87 :forward                              ; w
                   39 :right                                ; right arrow
                   68 :right                                ; d
                   40 :backward                             ; down arrow
                   83 :backward                             ; s
                   nil)]
      {:state :move
       :dir dir})))

(defn on-keydown [e]
  (js/console.log "keydown" e)
  (if-let [action (key-code->action (.-keyCode e))]
    (chsk-send! [:player/action
                 (assoc action :stop? false)])))

(defn on-keyup [e]
  (if-let [action (key-code->action (.-keyCode e))]
    (chsk-send! [:player/action
                 (assoc action :stop? true)])))

;; ----------
;; Rendering

(def tile-size 20)
(def half-tile-size (/ tile-size 2))

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

(defn render-hud [game player]
  (let [{:keys [hp max-hp]} player
        hp-percent (* 100 (/ hp max-hp))]
    [:div#game-hud
     [:div.player-state
      [:div "HP"]
      [:div.progress
       [:div {:style         {:width (str hp-percent "%")}
              :role          "progressbar"
              :aria-valuenow hp
              :aria-valuemin 0
              :aria-valuemax max-hp
              :class         (str "progress-bar progress-bar-"
                                  (condp < hp-percent
                                    60 "success"
                                    30 "warning"
                                    0 "danger"))}
        ;hp
        ]]]
     [:div.game-menu
      [:button.btn.btn-default
       {:on-click quit-game}
       "Quit"]]]))

(defn- seen? [{:keys [left right top bottom] :as visible-bounds} x y]
  (and (>= x left)
       (<= x right)
       (>= y top)
       (<= y bottom)))

(defn player-rotation [{:keys [direction] :as player}]
  (case direction
    [0 1] 0
    [1 1] 45
    [1 0] 90
    [1 -1] 135
    [0 -1] 180
    [-1 -1] -135
    [-1 0] -90
    [-1 1] -45))

(defn player-dir [player]
  (let [ts2 (* tile-size 2)
        rx (* ts2 (js/Math.cos (/ js/Math.PI 3)))
        ry (* ts2 (js/Math.sin (/ js/Math.PI 3)))
        p1 [rx ry]
        p2 [(- rx) ry]]
    [:path.player-dir {:d (str "M0 0"
                               "L" (first p1) " " (last p1)
                               "Q0 " (+ half-tile-size tile-size) " " (first p2) " " (last p2)
                               "z")}]))

(defn render-players [{:keys [player-id] :as db}
                      {:keys [players] :as game}
                      visible-bounds]
  (for [[id player] players
        :let [{:keys [x y]} player
              [w h] (to-view x y)]
        :when (seen? visible-bounds x y)]
    [:g {:key       id
         :transform (str "translate(" (+ w half-tile-size) " " (+ h half-tile-size) ")"
                         "rotate(" (player-rotation player) ")"
                         )
         :class     (str/join " "
                              (cond-> ["player"]
                                      (= player-id (:player-id player)) (conj "you")
                                      (:player-id player) (conj "live")))}
     (player-dir player)
     ;[:rect {:x 0 :y 0 :width tile-size :height tile-size}]
     [:circle.player-icon {:r half-tile-size}]]))

(defn top-transform [{:keys [x y] :as player} sight]
  (let [w (* tile-size x)
        h (* tile-size y)
        r (* tile-size sight)]
    (str "translate("
         (- r w)
         " "
         (- r h)
         ")")))

(defn svg-defs [player sight]
  [:defs
   [:radialGradient#sight-grad {:cx "50%"
                                :cy "50%"
                                :r  "50%"
                                :fx "50%"
                                :fy "50%"}
    [:stop {:offset 0.75
            :style  {:stop-color "white"}}]
    [:stop.transparent {:offset 1
                        :style  {:stop-color "black"}}]]
   [:radialGradient#player-sight-grad {:cx "50%"
                                       :cy "50%"
                                       :r  "75%"}
    [:stop {:offset 0
            :style  {:stop-color "#FF5722"}}]
    [:stop.transparent {:offset 1
                        :style  {:stop-color "white"}}]]
   [:mask#sight-mask
    [:circle {:cx   (* (:x player) tile-size)
              :cy   (* (:y player) tile-size)
              :r    (* sight tile-size)
              :fill "url(#sight-grad)"}]]])

(defn reg-keyboard []
  (js/window.addEventListener "keydown" on-keydown)
  (js/window.addEventListener "keyup" on-keyup))

(defn unreg-keyboard []
  (js/window.removeEventListener "keydown" on-keydown)
  (js/window.removeEventListener "keyup" on-keyup))

(defonce log-visible (atom true))

(defn render-log [game-started log]
  [:div#battle-log {:class (when-not @log-visible "log-hidden")}
   [:h2#log-header "Battle log"
    [:button.btn.btn-default {:on-click #(swap! log-visible not)}
     (if @log-visible "Hide" "Show")]]
   [:div#log-entries
    (map-indexed
      (fn [index {:keys [ts message]}]
        [:div.row {:key index}
         [:div.ts.col-xs-2 ts]
         [:div.col-xs-10 message]])
      log)]])

(defn render-field* []
  (let [{:keys [game player-id game-player-id]} @db
        {:keys [sight players]} game
        player (get-in game [:players game-player-id])
        visible-bounds (get-visible-bounds game player)]
    (println "render-field player" game-player-id player)
    [:div#game-field
     [:svg {:width  600
            :height 600}
      (svg-defs player sight)
      [:g {:transform (top-transform player sight)
           :style     {:mask "url(#sight-mask)"}}
       [:g.tile-group
        (render-tiles game visible-bounds)]
       [:g.player-group
        (render-players @db game visible-bounds)]]]
     (render-hud game player)
     (render-log (:game-started game) (:log game))
     ]))

(def render-field
  (with-meta render-field*
             {:component-did-mount    reg-keyboard
              :component-will-unmount unreg-keyboard}))