(ns aoc2015.day_2
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2015/day_2.txt"))
(def processed (str/split-lines raw))

(defn format-input
  [input]
  (map (fn [x] (sort (map #(Integer/parseInt %) x))) (map #(str/split % #"x") input)))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn calc-prism-total
  [[a b c]]
  (let [combos (map #(reduce * %) [[a b] [b c] [a c]])
        multiplied (map #(* 2 %) combos)]
    (+ (reduce * (take 2 [a b c])) (reduce + multiplied))))

(defn p1
  [list]
  (reduce + (map #(calc-prism-total %) list)))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn calculate-individual-total
  [present]
  (+ (* (reduce + (take 2 present)) 2) (reduce * present)))

(defn p2
  [list]
  (reduce + (map #(calculate-individual-total %) list)))
