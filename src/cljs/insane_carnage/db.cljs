(ns insane-carnage.db
  (:require [reagent.core :as reagent :refer [atom]]))

(defonce db
         (atom {:player-name ""
                :player-id   (random-uuid)
                :game-id     nil
                :game-state  :setup                         ; setup|wait|run|over
                }))