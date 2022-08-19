(ns aoc2016.day_2
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2016/day_2_practice.txt"))
(def processed (str/split-lines raw))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn translate-output
  [output]
  (map #(case %
          [0 0] 1
          [0 1] 4
          [0 2] 7
          [1 0] 2
          [1 1] 5
          [1 2] 8
          [2 0] 3
          [2 1] 6
          [2 2] 9) output))

(defn determine-position-1
  [position dir]
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

(defn determine-code
  [inp]
  (Integer/parseInt (str/join (translate-output (map #(parse-code %) inp)))))
;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn valid-range?
  [curr]
  (or (cond (zero? (second curr))
            (if (and (>= (first curr) -2) (<= (first curr) 2)) true false)
            (= (second curr) 1)
            (if (and (>= (first curr) -1) (<= (first curr) 1)) true false)
            (= (second curr) 2)
            (if (zero? (first curr)) true false)
            :else
            false)
      (cond (zero? (first curr))
            (if (and (>= (second curr) -2) (<= (second curr) 2)) true false)
            (= (first curr) 1)
            (if (and (>= (second curr) -1) (<= (second curr) 1)) true false)
            (= (first curr) 2)
            (if (zero? (second curr)) true false)
            :else
            false)))

(defn determine-position-2
  [position dir]
  (println position dir)
  (case (str dir)
    "D" [(first position) (if (valid-range? [(first position) (+ (second position) 1)]) (+ (second position) 1) (second position))]
    "U" [(first position) (if (valid-range? [(first position) (- (second position) 1)]) (- (second position) 1) (second position))]
    "L" [(if (valid-range? [(- (first position) 1) (second position)]) (- (first position) 1) (first position)) (second position)]
    "R" [(if (valid-range? [(+ (first position) 1) (second position)]) (+ (first position) 1) (first position)) (second position)]))

(defn translate-output-2
  [output result]
  (println result)
  (case output
    [0 0] "7"
    [1 0] "8"
    [2 0] "9"
    [0 1] "3"
    [1 1] "4"
    [0 -2] "D"
    [1 -1] "C"
    [-1 1] "2"
    [-1 -1] "A"
    [0 -1] "B"
    [0 2] "1"
    [-1 0] "6"
    [-2 0] "5"))

(defn parse-code-2
  [code start]
  (loop [c code
         position start
         output ""
         dir (first c)
         result [""]]
    (let [new-pos (determine-position-2 position dir)
          new-out (translate-output-2 new-pos result)
          new-output (if (= output new-out) "" new-out)]
      (if (empty? c)
        [(last result) position]
        (recur (rest c)
               new-pos
               new-output
               (first (str/join (rest c)))
               (concat result new-output))))))

(defn determine-code-2
  [inp]
  (println inp)
  (loop [i inp
         inst [-2 0]
         curr (parse-code-2 (first inp) inst)
         r []]

    (if (empty? i)
      (str/join r)
      (recur (rest i)
             (second curr)
             (parse-code-2 (first (rest i)) (second curr))
             (concat r (str (first curr)))))))



