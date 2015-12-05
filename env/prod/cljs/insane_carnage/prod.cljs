(ns insane-carnage.prod
  (:require [insane-carnage.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
