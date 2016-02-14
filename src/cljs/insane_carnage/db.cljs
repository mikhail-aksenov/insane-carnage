(ns insane-carnage.db
  (:require [reagent.core :as reagent :refer [atom]]
            [goog.net.cookies :as cookie]
            [insane-carnage.resources :as resources]))

(enable-console-print!)

(defn- get-player-id []
  (let [id (cookie/get "player-id" (random-uuid))]
    (cookie/set "player-id" id)
    id))

(defn- get-player-name []
  (let [name (cookie/get "player-name" (rand-nth resources/names))]
    (cookie/set "player-name" name)
    name))

(defonce db
         (atom {:player-name (get-player-name)
                :player-id   (get-player-id)
                :unit-id     nil
                :game-id     nil
                :state       :setup                      ; setup|waiting|running|over
                :game        nil
                }))