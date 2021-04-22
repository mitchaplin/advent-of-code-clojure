(ns advent_2016_day_7
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2016/day_6_practice.txt"))
(def processed (str/split-lines raw))

(defn convert-lists
  [item]
  (map #(str/split % #"\[") (str/split item #"\]")))

(defn explode [coll n]
  (reduce (fn [colls xs]
            (map #(if %2 (conj %1 %2) %1)
                 colls
                 (concat xs (repeat nil))))
          (repeat n [])
          (partition-all n coll)))

(defn contains-abba?
  [line]
  (or (some true? (map (fn [[a b c d]]
                         (and (= a d)
                              (= b c)
                              (not (= a b))))
                       (partition 4 1 line))) false))

(defn format-lists
  [item]
  (explode (flatten (convert-lists item)) 2))

(defn finalize
  []
  (map #(format-lists %) processed))

(defn calc-count
  [s c]
  (if (and (some true? (map contains-abba? (first (first s))))
           (every? false? (map contains-abba? (second (first s)))))
    (inc c)
    c))

(defn check-pals
  []
  (loop
    [s (finalize)
     count 0]
    (if (empty? s)
      count
      (recur
        (rest s)
        (calc-count s count)))))

;; Part 2
(defn contains-aba?
  [[supernets hypernets]]
  (let [nets (apply concat (map #(partition 3 1 %) hypernets))
        supers (apply concat (map #(partition 3 1 %) supernets))]
    (map (fn [hypernet]
           (some true?
                 (map (fn [[a b c]]
                        (and
                          (or
                            (= (list b a b) hypernet)
                            (= (list b c b) hypernet))
                          (not= (list a b c) hypernet)))
                      supers)))
         nets)))

(defn calc-count-2
  [s c]
  (let
    [x (every? true? (apply concat (map contains-aba? s)))
     _ (println x s c)]
    (if x
      (inc c)
      c)))

(defn check-pals-2
  []
  (loop
    [s (finalize)
     count 0]
    (if (empty? s)
      count
      (recur
        (rest s)
        (calc-count-2 s count)))))