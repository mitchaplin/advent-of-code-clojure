(ns advent_2021_day_3
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2021/day_2.txt"))
(def processed (map #(str/split % #" ") (str/split-lines raw)))



