(ns aoc2020.day_3
  (:require [clojure.string :as str]))

(def practice ["..##......."
               "#...#...#.."
               ".#....#..#."
               "..#.#...#.#"
               ".#...##..#."
               "..#.##....."
               ".#.#.#....#"
               ".#........#"
               "#.##...#..."
               "#...##....#"
               ".#..#...#.#"])
(def raw (slurp "resources/2020/day_3.txt"))
(def processed (str/split raw #"\n"))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn calc-wrap
  [pointer x len]
  (let [updated-pointer (+ x pointer)]
    (mod updated-pointer len)))

(defn check-collision
  [pointer d]
  (if (= (get d pointer) \#)
    1
    0))

(defn calc-trees
  [over down data]
  (let [line-length (count (first data))]
    (loop
      [pointer 0
       tree-count 0
       d data]
      (do (println pointer tree-count d)
          (if (empty? d)
            tree-count
            (recur (calc-wrap pointer over line-length)
                   (+ (check-collision pointer (first d)) tree-count)
                   (nthrest d down)))))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(reduce * (map #(apply calc-trees %) [[1 1 processed]
                                      [3 1 processed]
                                      [5 1 processed]
                                      [7 1 processed]
                                      [1 2 processed]]))