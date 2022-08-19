(ns aoc2017.day_4
  (:require [clojure.string :as str]))

(def raw (str/split-lines (slurp "resources/2017/day_4.txt")))

(defn format-input
  []
  (->> raw
       (map #(str/split % #" "))
       (map #(map vec %))
       (map #(map sort %)) ;; part 2
       (map #(if (= (count %) (count (set %))) 1 0))
       (reduce +)))
