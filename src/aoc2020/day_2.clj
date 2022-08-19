(ns aoc2020.day_2
  (:require [clojure.string :as str]))

(def practice `(["1-3 a" "abcde"]
                ["1-3 b" "cdefg"]
                ["2-9 c" "ccccccccc"]))
(def raw (slurp "resources/2020/day_2.txt"))
(def processed (map #(str/split % #": ") (str/split raw #"\n")))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn check-valid
  [p]
  (let
    [input (str/split (first p) #" ")
     v (apply char (second input))
     nums (map #(Integer/parseInt %) (str/split (first input) #"-"))
     r (range (first nums) (inc (second nums)) 1)
     f (frequencies (second p))]
    (if (true? (utils/in? r (second (find f v))))
      1
      0)))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn valid-logic
  [n s]
  (= (count (set (map #(utils/in? s %) n))) 2))

(defn check-valid-2
  [p]
  (let
    [input (str/split (first p) #" ")
     v (second input)
     nums (map #(Integer/parseInt %) (str/split (first input) #"-"))
     s (map inc (indexes-of v (str/split (second p) #"")))]
    (if (true? (valid-logic nums s))
      1
      0)))

(defn retrieve-valid-passwords
  []
  (loop [p processed
         count 0]
    (if (empty? p)
      count
      (recur (rest p)
             (+ count (check-valid-2 (first p)))))))

