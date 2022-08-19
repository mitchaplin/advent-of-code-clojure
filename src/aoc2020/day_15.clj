(ns aoc2020.day_15
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2020/day_15.txt"))
(def processed (str/split-lines raw))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defn- find-numbers
  [lines]
  (map #(Integer/parseInt %) (re-seq #"\d+" (first lines))))

(defn- find-position
  [target numbers]
  (let [history (zipmap (butlast numbers) (range 1 (count numbers)))]
    (loop [idx (count numbers)
           lnum (last numbers)
           hist history]
      (let [firstspoken (not (contains? hist lnum))
            nnum (if firstspoken
                   0
                   (- idx (hist lnum)))]
        (cond
          (= idx target)
          lnum

          :else
          (recur (inc idx) nnum (assoc hist lnum idx)))))))

(defn p1
  [input]
  (->> input
       (find-numbers)
       (find-position 2020)))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defn p2
  [input]
  (->> input
       (find-numbers)
       (find-position 30000000)))