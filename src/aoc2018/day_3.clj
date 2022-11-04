(ns aoc2018.day_3
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(def formatted-input
  (->> (slurp "resources/2018/day_3.txt")
       (str/split-lines)
       (map #(str/split % #"@ "))
       (map #(str/split (second %) #": "))))

(defn parse-inputs
  [inp]
  (let [[x1 x2] (map #(Integer/parseInt %) (map str (str/split (first inp) #",")))
        [y1 y2] (map #(Integer/parseInt %) (map str (str/split (second inp) #"x")))]
    (combo/cartesian-product (range x1 (+ x1 y1)) (range x2 (+ x2 y2)))))

(def c (mapcat #(parse-inputs %) formatted-input))

(def freq-map (frequencies c))

(defn part-1
  []
  (count (remove #(>= 1 %) (vals freq-map))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(def formatted-input-number
  (->> (slurp "resources/2018/day_3.txt")
       (str/split-lines)
       (map #(str/split % #" @ "))))

(defn split-input-number
  []
  (let [x (map #(list (first %) (str/split (second %) #": ")) formatted-input-number)]
    x))

(defn parse-inputs-2
  [[x y]]
  (let [[x1 x2] (map #(Integer/parseInt %) (map str (str/split (first y) #",")))
        [y1 y2] (map #(Integer/parseInt %) (map str (str/split (second y) #"x")))]
    (list x (combo/cartesian-product (range x1 (+ x1 y1)) (range x2 (+ x2 y2))))))

(def point-map (map #(parse-inputs-2 %) (split-input-number)))

(defn get-freqs
  [point]
  (get freq-map point))

(defn part-2
  []
  (loop
    [l point-map]
    (if (every? #(= % 1) (map #(get-freqs %) (second (first l))))
      (ffirst l)
      (recur (rest l)))))