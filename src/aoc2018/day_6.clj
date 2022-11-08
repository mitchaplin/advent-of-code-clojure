(ns aoc2018.day-6
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))
(def raw (str/split-lines (slurp "resources/2018/day_6.txt")))
(def central-points (map #(list (Integer/parseInt (first %)) (Integer/parseInt (second %)))
                         (map #(str/split % #", ") raw)))

(def max-x (inc (apply max (map first central-points))))
(def max-y (inc (apply max (map second central-points))))
(def generated-points (combo/cartesian-product (range 0 (inc max-x))
                                               (range 0 (inc max-y))))
;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn get-manhatten-distance
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn get-manhatten-distance-totals
  [gp]
  (let [sorted-man-dists (sort-by first (map #(list (get-manhatten-distance gp %) %) central-points))
        _ (println sorted-man-dists)]
    (if (not= (ffirst sorted-man-dists) (first (second sorted-man-dists)))
      (second (first sorted-man-dists)))))


(def generated-boundary-points-set
  (let [x (filter #(or (= (first %) 0)
                       (= (first %) max-x)
                       (= (second %) 0)
                       (= (second %) max-y))
                  generated-points)]
    (set (remove nil? (map #(get-parsed-manhatten-distance-totals %) x)))))

(def filtered-points (set/difference (set central-points) generated-boundary-points-set))
(def initial-freq-map (reduce (fn [acc item] (assoc acc item 0)) {} filtered-points))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn get-parsed-manhatten-distance-totals
  [gp]
  (let [sorted-man-dists (sort-by first (map #(list (get-manhatten-distance gp %) %) central-points))]
    (if (< (reduce (fn [acc item] (+ acc (first item))) 0 sorted-man-dists) 10000)
      (second (first sorted-man-dists)))))

(defn iterate-through-points
  []
  (reduce (fn [acc item]
            (let [next-point (get-parsed-manhatten-distance-totals item)]
              (if (or (nil? next-point) (contains? generated-boundary-points-set next-point))
                acc
                (assoc acc next-point (inc (get acc next-point)))))) initial-freq-map generated-points))

