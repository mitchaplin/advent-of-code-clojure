(ns advent_2021_day_3
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2021/day_3_practice.txt"))
(def processed (map #(str/split % #" ") (str/split-lines raw)))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn pluck-value
  [v f]
  (if (f (val (first v)) (val (second v)))
    (ffirst v)
    (first (second v))))

(defn p1
  []
  (loop [p processed
         tot 0
         acc []]
    (if (<= (count (ffirst processed)) tot)
      (* (Long/parseLong (apply str (map #(pluck-value % <) acc)) 2)
         (Long/parseLong (apply str (map #(pluck-value % >) acc)) 2))
      (recur
        p
        (inc tot)
        (conj
          acc
          (frequencies
            (map #(nth (first %) tot) p)))))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn p2
  [g]
  (loop [p processed
         tot 0
         acc []]
    (if (<= (count (ffirst processed)) tot)
      acc
      (recur
        p
        (inc tot)
        (conj
          acc
          (frequencies
            (map #(nth (first %) tot) p)))))))