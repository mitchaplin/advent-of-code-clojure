(ns aoc2018.day_2
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]
            [clojure.math.combinatorics :as combo]))

(def raw (slurp "resources/2018/day_2.txt"))
(def processed (str/split-lines raw))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn total-dupes
  [d acc]
  (let
    [calc (list (get (clojure.set/map-invert d) 2)
                (get (clojure.set/map-invert d) 3))]
    (list (if (not (nil? (first calc)))
            (inc (first acc))
            (first acc))
          (if (not (nil? (second calc)))
            (inc (second acc))
            (second acc)))))

(defn get-checksum
  []
  (loop
    [acc `(0 0)
     l (map #(frequencies %) processed)]
    (if (empty? l)
      (* (first acc) (second acc))
      (recur (total-dupes (first l) acc)
             (rest l)))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn calc-match
  [a b]
  (if (= a b) a ""))

(defn compare-letters
  [letter current]
  (loop
    [matches ""
     t current
     l letter]
    (if (empty? l)
      matches
      (recur
        (str matches (calc-match (first t)
                                 (first l)))
        (rest t)
        (rest l)))))

(defn get-common-letters
  []
  (let [x (combo/combinations processed 2)]
    (last (sort-by count (map #(compare-letters (first %) (second %)) x)))))



