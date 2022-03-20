(ns advent_2020_day_10
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(def raw (slurp "resources/2020/day_10.txt"))
(def processed (vec (sort (map #(Long/parseLong %) (str/split-lines raw)))))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn find-jumps
  []
  (apply * (vals (frequencies (map #(Math/abs (- (first %) (second %)))
                                   (partition 2 1 processed))))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn check-idk
  [coll f l]
  (clojure.set/subset? #{f l} (set coll)))

(defn part-one-works?
  [p]
  (clojure.set/subset? (set (map #(Math/abs (- (first %) (last %)))
                                 (partition 2 1 p))) #{1 2 3}))

(defn check-combos
  [line f l]
  (let [c (combo/subsets line)
        clean (remove #(< (count %) 2) c)
        valid (filter #(and (check-idk % f l) (part-one-works? %)) clean)]
    valid))

(defn parse-combos
  [line]
  (let [f (first line)
        l (last line)
        ct (count line)]

    (cond (< ct 2)
          line

          :else
          (check-combos line f l))))

(defn sad
  [d]
  (reduce
    (fn [acc n]
      (let [prev (peek (peek acc))]
        (if (and prev (= -1 (- prev n)))
          (update acc (dec (count acc)) conj n)
          (conj acc [n]))))
    []
    d))

(defn cycle-data
  []
  (apply * (map count (map parse-combos (sad processed)))))