(ns insane-carnage.util)

(defn foo-cljx [x]
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn generateMap [width length]
  (let [generateRow (fn [w]
                      (let [conv {0 "." 1 "," 2 "¸"}]
                        (map #(conv %) (take w (repeatedly #(rand-int 3))))))]
    (take length (repeatedly #(generateRow width)))))