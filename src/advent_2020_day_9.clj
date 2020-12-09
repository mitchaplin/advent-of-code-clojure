(ns advent_2020_day_9
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def raw (slurp "resources/2020/day_9_practice.txt"))
(def processed (map #(Long/parseLong %) (str/split-lines raw)))

(defn check-inclusion?
  [d n]
  (utils/in? d n))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn calc-encoding-error
  [preamble start]
  (loop [c start]
    (let [s (take (inc preamble) (drop c processed))
          d (map #(reduce + %) (combo/permuted-combinations (butlast s) 2))
          n (last s)]
      (if (nil? (check-inclusion? d n))
        n
        (recur (inc c))))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn find-contiguous-error-seq
  [n t minimum]
  (let [final (sort (loop
                      [dataset processed
                       min-length minimum]
                      (let [new-range (take min-length dataset)
                            sum (apply + new-range)]
                        (cond
                          ;sum is eq to target
                          (= sum t)
                          new-range
                          ;sum is less than target, bump min-len
                          (< sum t)
                          (recur dataset (inc min-length))
                          ;move on to next item in data
                          :else
                          (recur (rest dataset) minimum)))))]
    (+ (first final)
       (last final))))