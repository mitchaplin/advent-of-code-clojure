(ns advent_2021_day_2
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2021/day_2.txt"))
(def processed (map #(str/split % #" ") (str/split-lines raw)))

(defn update-position
  [current-position inst]
  (let [amount (Integer/parseInt (second inst))]
    (cond
      (= (first inst) "forward")
      [(+ (first current-position) amount)
       (+ (second current-position) (* (last current-position) amount))
       (last current-position)]

      (= (first inst) "down")
      [(first current-position)
       (second current-position)
       (+ (last current-position) amount)]

      (= (first inst) "up")
      [(first current-position)
       (second current-position)
       (- (last current-position) amount)])))

(defn map-instructions
  []
  (loop
    [loc [0 0 0]
     instructions processed]
    (if (empty? instructions)
      (reduce * (list (first loc) (second loc)))
      (recur
        (update-position loc (first instructions))
        (rest instructions)))))

