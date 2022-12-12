(ns aoc2022.day-10
  (:require
    [clojure.string :as str]))

(def raw (map #(str/split % #" ") (utils/read-file "resources/2022/day_10.txt")))

(defn update-points
  [points cycle val previous-val]
  (conj points [cycle previous-val] [(inc cycle) (+ val previous-val)]))

(defn parse-inst
  [inst points cycle]
  (let [previous-val (second (points (dec cycle)))]
    (if (= (count inst) 1)
      [(inc cycle) (conj points [cycle previous-val])]
      [(+ cycle 2) (update-points points cycle (utils/parse-int (second inst)) previous-val)])))

(defn calculate-total
  [is-p1?]
  (loop [s raw
         cycle 1
         points [[1 1]]]
      (if (empty? s)
       (if is-p1? (into [] (map (fn [[x y]] [(inc x) y]) points))
                 (into [] (rest points)))
       (let [[new-cycle new-points] (parse-inst (first s) points cycle)]
         (recur
           (rest s)
           new-cycle
           new-points)))))

(defn part-1
  []
  (let [total (calculate-total true)]
    (reduce + (map #(reduce * %) [(total (dec 20))
                                  (total (dec 60))
                                  (total (dec 100))
                                  (total (dec 140))
                                  (total (dec 180))
                                  (total (dec 220))]))))

(defn part-2
  []
  (map #(println (map-indexed (fn [i [x y]] (if (utils/in? (range (dec y) (+ 2 y)) (inc i)) "#" ".")) %))
       (partition 40 (calculate-total false))))