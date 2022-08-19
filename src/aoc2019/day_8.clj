(ns aoc2019.day_8
  (:require [clojure.string :as str]))

(def raw (slurp "resources/day_8.txt"))
(def input (map #(Integer/parseInt %) (str/split (str/trim-newline raw) #"")))


(println (partition 6 (map #(filter (fn [x] (= 0 x)) %)
                           (partition 6 6 (partition 25 input)))))

(println (partition 6 (map #(reduce +
                                    (map (fn [group]
                                           (count (filter (fn [x] (= 1 x)) group))) %))
                           (partition 6 6 (partition 25 input)))))

(println (partition 6 (map #(reduce +
                                    (map (fn [group]
                                           (count (filter (fn [x] (= 2 x)) group))) %))
                           (partition 6 6 (partition 25 input)))))

(partition 25 (loop
                [idx 0
                 acc []
                 data (partition 150 input)]
                (println idx, acc)
                (if (> idx 149)
                  acc
                  (recur (inc idx) (conj acc (or (first (remove #{2} (map #(nth % idx) data))) 2)) data))))