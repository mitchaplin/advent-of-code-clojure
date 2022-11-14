(ns aoc2018.day_8
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2018/day_8.txt"))


(def processed
  (->> (str/split raw #" ")
       (map #(Integer/parseInt %))))

(defn partition-keep-remainder
  [n col]
  (if (zero? n)
    []
    (let [elements (partition-all (quot (count col) n) col)]
      (concat
        (drop-last 2 elements)
        (vector (flatten (take-last 2 elements)))))))


(defn coerce-data
  [c]
  (when-not
      (empty? c)
      (let [x (take (- (count c) (second c)) c)
            acc (take-last (second c) c)]
          (list (first x) (nthrest x 2) acc))))

(defn p1
  [c]
  (let [[num-children meta total] (coerce-data c)
        acc total]
    (let [new-items (partition-keep-remainder num-children meta)]
      (if (empty? new-items)
        (reduce + acc)
        (+ (reduce + acc)
           (reduce + (map p1 new-items)))))))
