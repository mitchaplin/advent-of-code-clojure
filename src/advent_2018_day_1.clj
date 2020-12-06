(ns advent_2018_day_1
  (:require [clojure.string :as str]))

(def practice `(1 -2 3 1 1 -2))
(def raw (slurp "resources/2018/day_1.txt"))
(def processed (map #(Integer/parseInt %) (str/split raw #"\n")))

(defn duplicates
  [input]
  (loop [acc 0
         dupe-list #{0}
         [f & freqs] input]
    (let
      [new-nums (+ acc f)]
      (if (dupe-list new-nums)
        new-nums
        (recur new-nums (conj dupe-list new-nums) freqs)))))

(defn get-frequency
  []
  (->> processed
       (cycle)
       (duplicates)))