(ns insane-carnage.field
  (:require [reagent.core :as reagent :refer [atom]]
            [accountant.core :as accountant]
            [clojure.string :as str]
            [insane-carnage.db :refer [db]]
            [insane-carnage.talk :refer [chsk-send!]]
            [cljs-time.core :as time]
            [insane-carnage.engine :as engine]
            [insane-carnage.game :as game]))

(def move-key-map
  {37 :turn-left                                            ; left arrow
   65 :turn-left                                            ; a
   39 :turn-right                                           ; right arrow
   68 :turn-right                                           ; d
   38 :move-ahead                                           ; up arrow
   87 :move-ahead                                           ; w
   40 :move-back                                            ; down arrow
   83 :move-back                                            ; s
   32 :hit                                                  ; space
   })

(def pressed-moves (atom #{}))

(defn key-down-code->action [code]
  (when-let [move (get move-key-map code)]
    (swap! pressed-moves conj move)
    (game/player-move move)))

(defn key-up-code->action [code]
  (when-let [move (get move-key-map code)]
    (let [next-move
          (or (first
                (swap! pressed-moves disj move))
              :none)]
      (game/player-move next-move))))

(defn on-keydown [e]
  (key-down-code->action (.-keyCode e)))

(defn on-keyup [e]
  (key-up-code->action (.-keyCode e)))

(def tile-size 20)
(def half-tile-size (/ tile-size 2))

(defn to-view [x y]
  [(* tile-size x) (* tile-size y)])

(defn get-visible-bounds [game unit meta]
  (let [{:keys [width height]} game
        {:keys [x y]} unit
        {:keys [sight]} meta]
    {:left   (max 0 (- x sight))
     :top    (max 0 (- y sight))
     :right  (min width (+ x sight))
     :bottom (min height (+ y sight))}))

(defn render-tiles [game visible-bounds]
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
  ;{:pre [(:hp player) (:max-hp player)]}
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
       {:on-click game/leave}
       "Leave"]]]))

(defn- seen? [{:keys [left right top bottom] :as visible-bounds} x y]
  (and (>= x left)
       (<= x right)
       (>= y top)
       (<= y bottom)))

(defn direction->rotation [{:keys [direction] :as player}]
  (case direction
    [0 1] 0
    [1 1] 45
    [1 0] 90
    [1 -1] 135
    [0 -1] 180
    [-1 -1] -135
    [-1 0] -90
    [-1 1] -45))

(defn circle-segment-d [cx cy r angle]
  (let [rx (* r (js/Math.sin angle))
        ry (* r (js/Math.cos angle))
        p1 [rx ry]
        p2 [(- rx) ry]]
    (str "M0 0"
         "L" (first p2) " " (last p2)
         "A" r " " r " " 0 " " 0 " " 0 " " (first p1) " " (last p1)
         "z")))

(defn player-dir [player]
  [:path.player-dir {:d (circle-segment-d 0 0 (* tile-size 2) (/ js/Math.PI 6))}])

(defmulti render-hit :type)

(defmethod render-hit :swordsman [player]
  [:path.hit {:d (circle-segment-d 0 0 (* tile-size 1.5) (/ js/Math.PI 4))}])

(defmethod render-hit :pikeman [player]
  (let [shown? (atom false)]
    (fn []
      (if shown?
        [:path]
        (do
          (reset! shown? false)
          [:path.hit {:d (circle-segment-d 0 0 (* tile-size 2.5) (/ js/Math.PI 12))}])))))

(defn show-hit? [unit tick]
  (when-let [hit (:hit unit)]
    (let [{:keys [type]} unit
          meta (engine/unit-meta type)
          ticks-passed (- tick (:started-at hit))
          hit-duration (get meta :hit-duration)]
      (if (>= hit-duration ticks-passed)
        (render-hit unit)
        [:path]))))

(defn render-hit-general [unit tick]
  (when (show-hit? unit tick)
    (render-hit unit)))

(defn render-units [{:keys [player-id] :as db}
                    {:keys [units tick] :as game}
                    visible-bounds]
  (for [[id unit] units
        :let [{:keys [x y]} unit
              [w h] (to-view x y)]
        :when (seen? visible-bounds x y)
        :let [dead? (engine/dead? unit)]]
    [:g {:key       id
         :transform (str "translate(" (+ w half-tile-size) " " (+ h half-tile-size) ")"
                         "rotate(" (- (direction->rotation unit)) ")")
         :class     (str/join " "
                              (cond-> ["player"]
                                      (= player-id (:player-id unit)) (conj "you")
                                      (:player-id unit) (conj "live")
                                      dead? (conj "dead")))}
     (player-dir unit)
     (render-hit-general unit tick)
     [:circle.player-icon {:r half-tile-size}]]))

(defn top-transform [{:keys [x y] :as unit} sight]
  (let [w (* tile-size x)
        h (* tile-size y)
        r (* tile-size sight)]
    [(- r w) (- r h)]))

(defn svg-defs [unit sight]
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
    [:circle {:cx   (* (:x unit) tile-size)
              :cy   (* (:y unit) tile-size)
              :r    (* sight tile-size)
              :fill "url(#sight-grad)"}]]])

(defn reg-keyboard []
  (println "reg-keyboard")
  (js/window.addEventListener "keydown" on-keydown)
  (js/window.addEventListener "keyup" on-keyup))

(defn unreg-keyboard []
  (js/window.removeEventListener "keydown" on-keydown)
  (js/window.removeEventListener "keyup" on-keyup))

(defonce log-visible (atom false))

(defn render-log [log]
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

(defn get-passsed-time [game]
  (- (.getTime (js/Date.))
     (.getTime (js/Date. (:game-started game)))))

(defn animated [comp updated? upd duration]
  (let [clear-timer
        (fn [this]
          (when-let [timer (aget this "animation-timer")]
            (js/clearInterval timer)
            (aset this "animation-timer" nil)))
        animation-fn
        (fn [this started props next-props]
          (fn []
            (let [passed (- (.getTime (js/Date.)) started)]
              (if (> passed duration)
                (clear-timer this)
                (upd this props next-props (/ passed duration))))))
        launch-animation
        (fn [this props next-props]
          (clear-timer this)
          (let [now (.getTime (js/Date.))
                callback (animation-fn this now props next-props)
                timer (js/setInterval callback)]
            (aset this "animation-timer" timer)))
        update
        (fn [this new-argv]
          (let [props (reagent/props this)
                next-props (reagent.impl.util/extract-props new-argv)]
            (when (updated? props next-props)
              (launch-animation this props next-props))))]
    (with-meta comp
      {:component-will-receive-props update
       :component-will-unmount clear-timer})))

(defn render-field* [{:keys [game unit x y]}]
  (let [meta (engine/unit-meta (:type unit))
        visible-bounds (get-visible-bounds game unit meta)]
    [:g {:transform (str "translate(" x "," y ")")
         :style     {:mask "url(#sight-mask)"}}
     [:g.tile-group
      (render-tiles game visible-bounds)]
     [:g.player-group
      (render-units @db game visible-bounds)]]))

(def render-field-animated
  (let [updated?
        (fn [{x1 :x y1 :y :as p1} {x2 :x y2 :y :as p2}]
          (or (not= x1 x2) (not= y1 y2)))
        upd
        (fn [this {x1 :x y1 :y} {x2 :x y2 :y} i]
          (let [node (reagent/dom-node this)
                nx (+ x1 (* i (- x2 x1)))
                ny (+ y1 (* i (- y2 y1)))
                transform (str "translate(" nx "," ny ")")]
            (println "upd" [x1 y1] [x2 y2] [nx ny])
            (.setAttribute node "transform" transform)))]
    (animated render-field* updated? upd 100)))

(def render-field render-field*)

(defn render-game* []
  (let [{:keys [game unit-id]} @db
        {:keys [sight]} game
        width (.. js/document -documentElement -clientWidth)
        height (.. js/document -documentElement -clientHeight)
        unit (get-in game [:units unit-id])
        meta (engine/unit-meta (:type unit))
        [x y] (top-transform unit (:sight meta))]
    ;(println "render-field" unit-id (:hit unit))
    [:div
     [:div#game-field
      [:svg {:width  width
             :height height}
       (svg-defs unit sight)
       [render-field {:game game :unit unit :x x :y y}]]]
     (render-hud game unit)
     (render-log (:log game))]))

(def render-game
  (with-meta render-game*
             {:component-did-mount    reg-keyboard
              :component-will-unmount unreg-keyboard}))
