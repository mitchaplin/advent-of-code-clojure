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
  [[[a b] [x y]]]
  (cond (= a x)
        (if (> y b)
          (combo/cartesian-product [x] (range b (inc y)))
          (combo/cartesian-product [x] (range y (inc b))))

        (= b y)
        (if (> x a)
          (combo/cartesian-product (range a (inc x)) [b])
          (combo/cartesian-product (range x (inc a)) [b]))

        :else
        false))

(defn apply-ranges
  []
  (map #(generate-range %)
       (filter-non-verticals processed)))

(defn get-vertical-ranges
  []
  (apply concat (apply-ranges)))

(defn part-1
  []
  (count (remove #(> 2 (second %)) (frequencies (get-vertical-ranges)))))