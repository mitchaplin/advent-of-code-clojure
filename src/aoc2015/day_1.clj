(ns aoc2015.day_1
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2015/day_1.txt"))
(def processed (str/split raw #""))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn calculate-frequencies
  [item-list]
  (let [[down up] (into `() (frequencies item-list))]
    (- (second up) (second down))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn update-tally
  [item]
  (cond (= item "(")
        inc
        (= item ")")
        dec))

(defn find-first-basement
  [item-list]
  (loop [total 0
         idx 0
         l item-list]
    (if (< total 0)
      idx
      (recur ((update-tally (nth l idx)) total)
             (inc idx)
             l))))
