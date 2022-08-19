(ns aoc2017.day_2
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def raw (slurp "resources/2017/day_2.txt"))

(def processed
  (->> raw
       (str/split-lines)
       (map #(str/split % #"\t"))
       (map #(map (fn [item] (Integer/parseInt item)) %))
       (map (fn [lst] (- (apply max lst) (apply min lst))))
       (reduce +)))

(defn check-divisible
  [line]
  (map #(if (zero? (rem (first %) (second %)))
          (/ (first %) (second %)) 0)
       (map #(sort > %)
            (combo/combinations line 2))))

(def processed-2
  (->> raw
       (str/split-lines)
       (map #(str/split % #"\t"))
       (map #(map (fn [item] (Integer/parseInt item)) %))
       (map #(check-divisible %))
       (flatten)
       (reduce +)))

