(ns aoc2022.day-1
  (:require [clojure.string :as str]))

(def raw
  (->> "resources/2022/day_1.txt"
       (slurp)))

(def processed (map #(str/split % #"\n") (str/split raw #"\n\n")))

(defn part-1
  []
  (apply max (map (fn [x] (reduce + (map #(Integer/parseInt %) x))) processed)))

(defn part-2
  []
  (reduce + (take 3 (reverse (sort (map (fn [x] (reduce + (map #(Integer/parseInt %) x))) processed))))))
