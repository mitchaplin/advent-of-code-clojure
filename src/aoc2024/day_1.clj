(ns aoc2024.day-1
  (:require
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]))
(def raw (slurp "resources/2024/day_1.txt"))
(def processed (str/split (str/replace raw #" " "") #"\n"))
;; (def transformed-ints (partition 2 (map (fn [item] (sort (map #(Integer/parseInt (str %)) item))) processed)))
(def transformed-ints (partition 2 (map (fn [item] (sort (map #(Integer/parseInt (str %)) item))) processed)))
transformed-ints
;; (->> transformed-ints
   ;; (map #(zipmap (first %) (second %))))
   ;; (map #(math/abs (reduce - %)))
   ;; (reduce +))
