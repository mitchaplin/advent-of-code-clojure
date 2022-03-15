(ns advent_2016_day_8
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2016/day_8_practice.txt"))
(def processed (map #(str/split % #" ") (str/split-lines raw)))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn rotate
  [i]
  (println "ROTO" i))

(defn shape
  [i]
  (println "SHAPE" i))

(defn parse
  []
  (map #(if (= (first %) "rotate")
          (rotate (rest %))
          (shape (rest %))) processed))
