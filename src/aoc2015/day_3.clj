(ns aoc2015.day_3
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2015/day_3.txt"))
(def processed (str/split (str/trim-newline raw) #""))

(defn determine-next-point [[x y] direction]
  (case direction
    "<" [(- x 1) y]
    ">" [(+ x 1) y]
    "^" [x (- y 1)]
    "v" [x (+ y 1)]))

(defn draw-points [processed]
  (loop [inp processed points [[0 0]]]
    (if (empty? inp)
      points
      (recur (rest inp) (conj points (determine-next-point (last points) (first inp)))))))

(defn partition-evens [processed]
  (loop [p processed acc [] count 0]
    (if (empty? p)
      acc
      (do
        (recur (rest p) (if (even? count) acc (conj acc (first p))) (inc count))))))

(defn partition-odds [processed]
  (loop [p processed acc [] count 0]
    (if (empty? p)
      acc
      (do
        (recur (rest p) (if (even? count) acc (conj acc (first p))) (inc count))))))

(count (set (into (draw-points (filter #(not-empty %) (partition-evens processed))) (draw-points (filter #(not-empty %) (partition-odds processed))))))