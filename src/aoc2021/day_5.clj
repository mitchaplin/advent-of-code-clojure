(ns aoc2021.day-5
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def processed
  (->> (slurp "resources/2021/day_5_practice.txt")
       (str/split-lines)
       (map #(str/split % #" -> "))
       (map #(map (fn [x] (str/split x #",")) %))
       (map #(map (fn [x] (map (fn [y] (utils/parse-int y)) x)) %))))

(defn filter-non-verticals
  [p]
  (partition 2 2 (apply concat (filter (fn [[x y]]
                                         (or (= (first x) (first y))
                                             (= (second x) (second y))))
                                       p))))

(defn generate-range
  [[[a b] [x y]] diagonal-fn]
  (cond (= a x)
        (if (> y b)
          (combo/cartesian-product [x] (range b (inc y)))
          (combo/cartesian-product [x] (range y (inc b))))

        (= b y)
        (if (> x a)
          (combo/cartesian-product (range a (inc x)) [b])
          (combo/cartesian-product (range x (inc a)) [b]))

        :else
        (diagonal-fn a x b y)))

(defn get-diagonal-ranges
  [x1 x2 y1 y2]
  (let [x-range (apply utils/inclusive-range (sort [x1 x2]))
        x-range (if (= (first x-range) x1) x-range (reverse x-range))
        y-range (apply utils/inclusive-range (sort [y1 y2]))
        y-range (if (= (first y-range) y1) y-range (reverse y-range))]
    (map vec (apply zipmap [x-range y-range]))))

(defn apply-ranges
  ([diagonal-fn]
   (map #(generate-range % diagonal-fn)
        processed))
  ([diagonal-fn filter-verticals]
   (map #(generate-range % diagonal-fn)
        (filter-verticals processed))))

(defn get-vertical-ranges
  [diagonal-fn p1]
  (if p1
    (apply concat (apply-ranges diagonal-fn filter-non-verticals))
    (apply concat (apply-ranges diagonal-fn))))

;PART 1 & 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defn count-overlapping-points
  [d filter-verticals-part-1]
  (count (remove #(> 2 (second %))
                 (frequencies (get-vertical-ranges d filter-verticals-part-1)))))

;;(count-overlapping-points (constantly false) true) // Part 1
;;(count-overlapping-points get-diagonal-ranges false) // Part 2