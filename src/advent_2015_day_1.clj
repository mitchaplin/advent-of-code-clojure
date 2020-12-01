(ns advent_2015_day_1
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2015/day_1.txt"))
(def processed (str/split raw #""))

(defn calculate-frequencies [item-list]
  (- (or (second (first (frequencies item-list))) 0) (or (second (second (frequencies item-list))) 0)))

(def starting-list (nthrest processed 5))

(loop [position 5 input starting-list current ["(" "(" "(" "(" ")"]]
  (if (< (calculate-frequencies current) 0)
    (count current)
    (do
      (println (calculate-frequencies current) position input current)
      (recur (inc position) (rest input) (conj current (first input))))))
