(ns insane-carnage.db
  (:require [reagent.core :as reagent :refer [atom]]
            [goog.net.cookies :as cookie]))

(enable-console-print!)

(defn- get-player-id []
  (let [id (cookie/get "player-id" (random-uuid))]
    (cookie/set "player-id" id)
    id))

(defonce db
         (atom {:player-name ""
                :player-id   (get-player-id)
                :game-player-id nil
                :game-id     nil
                :game-state  :setup                         ; setup|wait|run|over
                :game        nil
                }))