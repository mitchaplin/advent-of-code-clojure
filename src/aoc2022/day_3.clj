(ns aoc2022.day-3
  (:require [clojure.set :as set]))

(def raw (utils/read-file "resources/2022/day_3.txt"))

(def processed (map #(list (subs % 0 (/ (count %) 2))
                           (subs % (/ (count %) 2) (count %)))
                    raw))

;(def values (apply hash-map (flatten (concat (map reverse (partition 2 2 (interleave (range 1 27) (->> (range (int \a) (inc (int \z))) (map char)))))
;                                             (map reverse (partition 2 2 (interleave (range 27 53) (->> (range (int \A) (inc (int \Z))) (map char)))))))))

(def values (apply str (concat "_" (->> (range (int \a) (inc (int \z))) (map char)) (->> (range (int \A) (inc (int \Z))) (map char)))))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn update-total
  [[f s]]
  (let [dupes (set/intersection (set f) (set s))
        new-total (reduce + (map #(.indexOf values (str %)) dupes))]
    new-total))

(defn part-1
  []
  (loop [p processed
         total 0]
    (if (empty? p)
      total
      (recur (rest p)
             (+ total (update-total (first p)))))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(def processed-2 (partition 3 3 raw))

(defn update-total-2
  [[f s l]]
  (let [dupes (set/intersection (set f)
                                (set s)
                                (set l))
        new-total (reduce + (map #(.indexOf values (str %)) dupes))]
    new-total))

(defn part-2
  []
  (loop [p processed-2
         total 0]
    (if (empty? p)
      total
      (recur (rest p)
             (+ total (update-total-2 (first p)))))))

