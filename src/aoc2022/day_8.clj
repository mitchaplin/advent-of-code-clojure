(ns aoc2022.day-8
  (:require
    [clojure.math.combinatorics :as combo]
    [clojure.set :as set]
    [clojure.string :as str]))

(def raw (utils/read-file "resources/2022/day_8.txt"))

(def processed (mapv #(mapv (fn [y] (utils/parse-int y)) %) (map #(str/split % #"") raw)))
(def max-length (dec (count processed)))

(def all-points (into [] (mapv vec (combo/cartesian-product (range (inc max-length)) (range (inc max-length))))))
(def non-edge-points (remove #(or (= (first %) 0)
                                  (= (second %) max-length)
                                  (= (first %) max-length)
                                  (= (second %) 0))
                             all-points))

(defn generate-columns
  [[x y]]
  (list (partition 2 (interleave (range 0 x) (repeat (if (> (/ max-length 2) x) (- max-length x) x) y)))
        (partition 2 (interleave (range (inc x) (inc max-length)) (repeat (if (> (/ max-length 2) x) (- max-length x) x) y)))
        (partition 2 (interleave (repeat (if (> (/ max-length 2) y) (- max-length y) y) x) (range 0 y)))
        (partition 2 (interleave (repeat (if (> (/ max-length 2) y) (- max-length y) y) x) (range (inc y) (inc max-length))))))

(defn get-values
  [p]
  (partition 2 (interleave (map #(map (fn [z] (map (fn [[x y]] ((nth processed y) x)) z)) %) (map generate-columns p)) p)))


;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn determine-vis
  [[x1 x2 y1 y2] [x y]]
  (let [xr ((nth processed y) x)]
    (if (or (< (apply max x1) xr)
            (< (apply max x2) xr)
            (< (apply max y1) xr)
            (< (apply max y2) xr))
      [x y]
      nil)))

(defn gather-points
  [v]
  (+
    (- (* (inc max-length) 4) 4)
    (count (remove nil? (map #(determine-vis (first %) (second %)) (get-values v))))))

;(gather-points non-edge-points)

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn tally-trees
  [s v]
  (loop [g s
         current-tallest 0
         total 0]
    (if (or (>= current-tallest v) (empty? g))
      total
      (recur (rest g)
             (first g)
             (inc total)))))

(defn determine-vis-2
  [[x1 x2 y1 y2] [x y]]
  (let [xr ((nth processed y) x)]
    (*
      (tally-trees (reverse x1) xr)
      (tally-trees x2 xr)
      (tally-trees (reverse y1) xr)
      (tally-trees y2 xr))))

(defn gather-points-2
  [v]
  (apply max (map #(determine-vis-2 (first %) (second %)) (get-values v))))

;(gather-points-2 non-edge-points)