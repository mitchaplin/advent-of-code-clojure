(ns aoc2020.day_5
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))

(def raw (slurp "resources/2020/day_5.txt"))
(def processed (str/split raw #"\n"))

(defn calc-seat
  [parts]
  (let [high (dec (math/expt 2 (count parts)))]
    (loop [p parts
           lo 0
           hi high]
      (let [gap (/ (+ lo hi 1) 2)]
        (cond
          (= (first p) \F) (recur (rest p) lo (- gap 1))
          (= (first p) \L) (recur (rest p) lo (- gap 1))
          (= (first p) \B) (recur (rest p) gap hi)
          (= (first p) \R) (recur (rest p) gap hi)
          (nil? (first p)) lo)))))

(defn get-id
  [pass]
  (let [row (take 7 pass)
        seat (drop 7 pass)]
    (+ (* 8 (calc-seat row))
       (calc-seat seat))))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn part-1
  []
  (->> processed
       (map get-id)
       sort
       last))

(defn find-gap
  [p]
  (let [[initial & lst] p]
    (loop [[l & remaining] lst
           init initial]
      (cond
        (nil? l) nil
        (= (inc init) l) (recur remaining l)
        :else (dec l)))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn part-2
  []
  (->> processed
       (map get-id)
       sort
       find-gap))