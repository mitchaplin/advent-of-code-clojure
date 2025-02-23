(ns aoc2024.day-1
  (:require
   [clojure.string :as str]))
(def raw (slurp "resources/202/day_1.txt"))
(def processed (str/split (str/replace raw #" " "") #"\n"))
(def transformed-ints (partition 2 (map (fn [item] (map #(Integer/parseInt (str %)) item)) processed)))

(->> transformed-ints
     (map (fn [x] (map #(reduce + %) x)))
     (map #(reduce - %))
     (reduce +))
