(ns aoc2021.day_7
  (:require [clojure.string :as str]))

(def raw (utils/load-edn "2021/day_7.edn"))

(defn average
  [coll]
  (int (Math/ceil (/ (reduce + coll) (count coll)))))

(defn find-fuel-consumption
  []
  (apply min (map (fn [x] (reduce + (map #(Math/abs (- (Math/abs %) x))
                                         raw)))
                  (range (apply min raw) (apply max raw)))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn find-fuel-consumption
  []
  (apply min (map (fn [x] (reduce + (map #(reduce + (range 1 (inc (Math/abs (- (Math/abs %) x)))))
                                         raw)))
                  (range (apply min raw) (apply max raw)))))