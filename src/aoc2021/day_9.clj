(ns aoc2021.day_9
  (:require [clojure.string :as str]))


(def raw (slurp "resources/2021/day_9_practice.txt"))
(def processed (str/split-lines raw))
(def top-pad (vec (cons processed (repeat (count (first processed)) nil))))
(def padded (partition 3 1 (vec (conj top-pad (vector (repeat (count (first top-pad)) nil))))))

(defn pluck-height-from-map
  [x y part]
  (list

    (defn pluck-height-from-map
      [x y part]
      (list
        ;;(Integer/parseInt (str (get (get part (dec x)) (dec y)))) ;; top left

        (if (neg? (dec x)) nil (get y (get part (dec x)))) ;; left

        ;;  (Integer/parseInt (str (get (get part (dec x)) (inc y)))) ;; bottom left

        (get (inc y) (get part x)) ;; bottom

        ;; (Integer/parseInt (str (get (get part (inc x)) (inc y)))) ;; bottom right

        (if (> (inc x) (count part)) nil (get y (get part (inc x)))) ;; right

        ;; (Integer/parseInt (str (get (get part (inc x)) (dec y)))) ;; top right

        (str (get (dec y) (get part x))) ;; top

        (get y (get part x)))))) ;; middle

(defn tally-results
  [results]
  (map #(if (> (apply min (butlast %)) (last %))
          (inc (last %))
          0)
       results))

(defn generate-heightmap
  [part]
  (loop
    [x 1
     results []]
    (if (<= (dec (count (first part))) x)
      (tally-results results)
      (recur (inc x)
             (conj results (pluck-height-from-map x 1 part))))))

(defn p1
  []
  (reduce + (apply concat (map #(generate-heightmap (vec %)) padded))))