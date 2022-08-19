(ns aoc2020.day_1
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def practice `(1721

                 366     979
                 299
                 675
                 1456))

(def raw (slurp "resources/2020/day_1.txt"))
(def processed (map #(Integer. %) (str/split raw #"\n")))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn all-pairs
  [coll]
  (map #(list (first %)
              (second %)
              (+ (first %)
                 (second %)))
       (let [x (first coll)
             y (next coll)]
         (when y
           (lazy-cat
             (map (fn [z] [x z]) y)
             (all-pairs y))))))

(defn find-answer
  []
  (loop
    [p (all-pairs processed)]
    (if (= (last (first p)) 2020)
      (* (ffirst p) (second (first p)))
      (recur (rest p)))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn actually-all-pairs
  [practice]
  (set (map sort
            (map #(list (first %)
                        (second %)
                        (last %)
                        (reduce + %))
                 (combo/permuted-combinations practice 3)))))

(defn find-better-answer
  []
  (loop
    [p (actually-all-pairs processed)]
    (if (= (last (first p)) 2020)
      (reduce * (butlast (first p)))
      (recur (rest p)))))