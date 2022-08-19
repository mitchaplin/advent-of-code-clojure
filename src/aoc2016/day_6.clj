(ns aoc2016.day_6
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2016/day_6.txt"))
(def processed (str/split-lines raw))

(defn format-data
  []
  (loop
    [new-data []
     idx 0]
    (let [formatted (str/join (map #(get % idx) processed))]
      (if (<= (first (max (map count processed))) idx)
        new-data
        (recur
               (conj new-data formatted)
               (inc idx))))))

(defn get-code
  []
  (str/join (map #(first (first (sort-by second %)))
                 (map frequencies (format-data)))))
