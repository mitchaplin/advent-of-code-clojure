(ns advent_2018_day_8
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2018/day_8_practice.txt"))


(def processed
  (->> (str/split raw #" ")
       (map #(Integer/parseInt %))))

(defn partition-keep-remainder
  [n col]
  (if (zero? n)
    col
    (let [elements (partition-all (quot (count col) n) col)]
      (concat (drop-last 2 elements) (vector (apply concat (take-last 2 elements)))))))

(defn coerce-data
  [c]
  (when-not
    (empty? c)
    (let [x (partition-all (- (count c) (second c)) c)]
      (if (empty? c)
        (list nil [] [])
        (list (ffirst x) (nthrest (first x) 2) (second x))))))

(defn p2
  [c]
  (let [c (coerce-data c)]
    (loop [acc []
           new-items (partition-keep-remainder (first c) (second c))]
      (if (or (empty? new-items) (nil? (first c)))
        acc
        (recur
          (concat acc (last c))
          (map p2 new-items))))))
