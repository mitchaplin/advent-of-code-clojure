(ns aoc2022.day-13
  (:require [clojure.string :as str]))

(def raw (map #(str/split % #"\n") (utils/split-blank-line (slurp "resources/2022/day_13_practice.txt"))))

(defn iterate-through-input
  [input]
  (loop [i input
         total 0]
    (if (empty? i)
      total)
    (let [[left right] (first i)]
      (cond (= (first left)"[")))))