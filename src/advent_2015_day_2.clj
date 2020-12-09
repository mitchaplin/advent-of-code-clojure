(ns advent_2015_day_2
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2015/day_2.txt"))
(def processed (str/split-lines raw))

(defn format-input
  [processed]
 (map #(conj (vec (map (fn [x] (Integer/parseInt x)) (str/split % #"x"))) (Integer/parseInt (first (str/split % #"x")))) processed))

(defn determine-min
  [present]
  (loop [p present acc []]
    (if (<= (count p) 1)
      (apply min acc)
      (do
        (recur (rest p)
               (conj acc (* (first p) (second p))))))))

(defn calc-present-total
  [present]
  (reduce + (conj (loop
                    [p present
                     acc []]
                    (if (<= (count p) 1)
                      (conj acc (determine-min present))
                      (recur (rest p)
                             (conj acc (* (* (first p) (second p)) 2))))))))

(defn calc-overall-total
  []
  (reduce + (loop
              [remaining (format-input processed)
               acc []]
              (if (empty? remaining)
                acc
                (do
                    (println acc)
                    (recur (rest remaining)
                           (conj acc (calc-present-total (first remaining)))))))))

(defn determine-and-calc-present-mins
  [present]
  (+ (reduce + (flatten (map (fn [x] (repeat 2 x))
                             (remove #(= (apply max (drop-last present)) %)
                                     (drop-last present))))) (apply * (drop-last present))))

(defn calc-square
  [[l w h]]
  (let
    [ [f s] (sort (list l w h))
     ribbon (* l w h)]
    (+ (+ f f) (+ s s) ribbon)))

(defn calc-efficient-present-total
  [processed]
  (reduce +
          (loop
            [remaining (format-input processed)
             acc []]
            (if (empty? remaining)
              acc
              (do
                (println acc (reduce + acc) remaining)
                (recur (rest remaining)
                       (conj acc (determine-and-calc-present-mins (first remaining)))))))))

