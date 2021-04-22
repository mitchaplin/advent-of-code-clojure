(ns advent_2020_day_13
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2020/day_13_practice_2.txt"))
(def processed (str/split-lines raw))

(def departure (Integer/parseInt (first processed)))
(def cleaned-times (map #(Integer/parseInt %)
                        (remove #(= % "x")
                                (str/split (second processed) #","))))

(def indexed-cleaned-times (map #(list (first %) (Integer/parseInt (second %)))
                                (remove #(= (second %) "x")
                                        (map-indexed vector (str/split (second processed) #",")))))


;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn get-totals
  []
  (map #(list (- (apply max %) departure)
              (* (- (apply max %) departure) (first %))
              (first %))
       (map #(reductions + (take (inc (Math/toIntExact (/ departure %)))
                                 (repeatedly (fn [] (+ % 0))))) cleaned-times)))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn series
  [series-id next-bus-id offset]
  (loop [position 0
         acc []]
    (if (= (count acc) 2)
      [acc (- (second acc) (first acc)) series-id]
      (if (zero? (rem (+ offset (* (+ offset series-id) position)) next-bus-id))
        (do
          (println position series-id)
          (recur (inc position) (conj acc (* (+ offset series-id) position))))
        (recur (inc position) acc)))))

(defn series-2
  [series-id next-bus-id offset]
  (loop [position 0
         acc []]
    (if (= (count acc) 2)
      [acc (- (second acc) (first acc)) series-id]
      (if (zero? (rem (+ offset (* series-id position)) next-bus-id))
        (do
          (println position series-id)
          (recur (inc position) (conj acc (* series-id position))))
        (recur (inc position) acc)))))

(defn cycle-items
  [data]
  (loop [d data
         start (second (first d))
         acc 0]
    (if (<= (count d) 1)
      acc
      (let [s1 (series start (second (second d)) (first (second d)))
            s2 (series-2 start (second (second d)) (first (second d)))
            next (second s2)
            _ (println start s1 s2 "TEST" (count d))]
        (recur (rest d)
               next
               (- (second (first s1)) (second s2)))))))