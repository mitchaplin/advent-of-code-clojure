(ns aoc2022.day-4
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))

(def raw (utils/read-file "resources/2022/day_4.txt"))

(def processed
  (->> raw
       (map #(str/split % #","))
       (map #(map (fn [x] (str/split x #"-")) %))))

(def generated-ranges
  (->> processed
       (map (fn [x] (map (fn [[a b]] (range (utils/parse-int a)
                                            (inc (utils/parse-int b))))
                         x)))))

(def range-sets (map (fn [[a b]] (list (set a)
                                       (set b)))
                     generated-ranges))

(defn update-total
  [total [b1 b2]]
  (if (or (set/subset? b1 b2)
          (set/subset? b2 b1))
    (inc total)
    total))

(defn update-total-p2
  [total [b1 b2]]
  (if (> (count (set/intersection b2 b1)) 0)
    (inc total)
    total))

;PART 1 & 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defn calc-assignment-pairs
  [fnc]
  (loop
    [t range-sets
     total 0]
    (if (empty? t)
      total
      (recur (rest t)
             (fnc total (first t))))))

;(calc-assignment-pairs update-total-p2)
;(calc-assignment-pairs update-total)
