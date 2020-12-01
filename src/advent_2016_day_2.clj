(ns advent_2016_day_2
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2016/day_2.txt"))
(def processed (str/split-lines raw))

(defn determine-position
  [position dir]
  (println position dir)
  (case (str dir)
    "D" [(first position) (if (<= (second position) 1) (+ (second position) 1) (second position))]
    "U" [(first position) (if (>= (second position) 1) (- (second position) 1) (second position))]
    "L" [(if (>= (first position) 1) (- (first position) 1) (first position)) (second position)]
    "R" [(if (<= (first position) 1) (+ (first position) 1) (first position)) (second position)]))

(defn parse-code
  [code]
  (loop [c code position [0 0] dir (first c)]
    (if (empty? c)
      position
      (recur (str/join (rest c))
             (determine-position position dir)
             (first (str/join (rest c)))))))

(defn translate-output [output] (map #(case %
                                        [0 0] 2
                                        [1 0] 3
                                        [2 0] 4
                                        [0 1] 6
                                        [1 1] 7
                                        [2 1] 8
                                        [3 1] 9
                                        [1 -1] 1
                                        [-1 1] 5
                                        [0 2] "A"
                                        [1 2] "B"
                                        [2 2] "C"
                                        [1 3] "D") output))
(defn determine-code
  [inp]
  (Integer/parseInt (str/join (translate-output (map #(parse-code %) inp)))))


