(ns advent_2021_day_1
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2021/day_1.txt"))
(def processed (map #(Integer/parseInt %) (str/split-lines raw)))

(defn get-counts
  []
  (count (filter #(< (first %) (second %))
              (partition 2 1 (map #(reduce + %) (partition 3 1 processed))))))


