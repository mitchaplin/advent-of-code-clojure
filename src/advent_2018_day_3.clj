(ns advent_2018_day_3
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def formatted-input
  (->> (slurp "resources/2018/day_3.txt")
       (str/split-lines)
       (map #(str/split % #"@ "))
       (map #(str/split (second %) #": "))))

(defn tweeze-inputs
  [inp]
  (let [[x1 x2] (map #(Integer/parseInt %) (map str (str/split (first inp) #",")))
        [y1 y2] (map #(Integer/parseInt %) (map str (str/split (second inp) #"x")))]
    (combo/cartesian-product (range x1 (+ x1 y1)) (range x2 (+ x2 y2)))))

(defn calculate-points
  []
  (loop
    [inp formatted-input
     points []]
    (if (empty? inp)
      (count (remove #(>= 1 %) (vals (frequencies (apply concat points)))))
      (recur (rest inp)
             (concat points (combo/cartesian-product (tweeze-inputs (first inp))))))))

(defn calculate-points-2
  []
  (loop
    [inp formatted-input
     points []]
    (if (empty? inp)
      (frequencies (apply concat points))
      (recur (rest inp)
             (concat points (combo/cartesian-product (tweeze-inputs (first inp))))))))

(defn isolate
  []
  (let [freqs (calculate-points-2)]
    (loop
      [inp formatted-input
       idx 0
       current (tweeze-inputs (first inp))
       nums (map #(get freqs %) current)]
      (if (nil? (some #(not= % 1) nums))
        (inc idx)
        (recur
          (rest inp)
          (inc idx)
          (tweeze-inputs (first (rest inp)))
          (map #(get freqs %) (tweeze-inputs (first (rest inp)))))))))








