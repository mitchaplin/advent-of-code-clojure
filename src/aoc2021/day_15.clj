(ns aoc2021.day_15
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def raw (slurp "resources/2021/day_15.txt"))
(defn convert-to-ints
  [line]
  (map #(Integer/parseInt %) (map str line)))

(def processed (map convert-to-ints (str/split-lines raw)))
(def x-max (count (first processed)))
(def y-max (count processed))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn add-point-to-directed-graph
  [direct-graph [x y]]
  (merge
    {[x y]
     {[(inc x) y]
      (when (not (>= (inc x) x-max))
        (nth (nth processed y) (inc x)))

      [x (inc y)]
      (when (not (>= (inc y) y-max))
        (nth (nth processed (inc y)) x))}}
    direct-graph))

(defn generate-graph-points
  []
  (combo/cartesian-product (range x-max) (range y-max)))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(def processed-2 (map #(list % %) (map convert-to-ints (str/split-lines raw))))
(defn update-if-needed
  [numlist]
  (map
    #(if (> (inc %) 9)
       1
       (inc %))
    numlist))

(defn update-board
  [dataset]
  (map #(list (concat (first %) (update-if-needed (second %))) (update-if-needed (second %))) dataset))

(defn update-board-2
  []
  (map first (nth (iterate update-board processed-2) 4)))

(defn generate-board
  []
  (let [x (update-board-2)
        a (concat x (map #(update-if-needed %) (take-last 100 x)))
        a (concat a (map #(update-if-needed %) (take-last 100 a)))
        a (concat a (map #(update-if-needed %) (take-last 100 a)))
        a (concat a (map #(update-if-needed %) (take-last 100 a)))]
    a))

(def board (generate-board))
(def x-max-2 (count (first board)))
(def y-max-2 (count board))

(defn generate-graph-points-2
  []
  (combo/cartesian-product (range x-max-2) (range y-max-2)))

(defn add-point-to-directed-graph-2
  [direct-graph [x y]]
  (merge
    {[x y]
     {[(inc x) y]
      (when (not (>= (inc x) x-max-2))
        (nth (nth board y) (inc x)))

      [x (inc y)]
      (when (not (>= (inc y) y-max-2))
        (nth (nth board (inc y)) x))}}
    direct-graph))