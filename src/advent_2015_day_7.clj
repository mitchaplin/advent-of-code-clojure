(ns advent_2015_day_7
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2015/day_7_practice.txt"))
(def processed (map #(str/split % #" ") (str/split-lines raw)))

(defn initialize-map
  []
  (reduce #(conj %1 {(keyword (last %2)) nil}) {} processed))

(defn format-instructions
  [])
