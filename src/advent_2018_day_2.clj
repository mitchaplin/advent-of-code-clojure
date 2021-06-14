(ns advent-2018-day-2
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(def practice
  ["fghij" "abcde" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])

(def raw (slurp "resources/2018/day_2.txt"))
(def processed (str/split-lines raw))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn total-dupes
  [d acc]
  (let
    [calc (list (get (clojure.set/map-invert d) 2)
                (get (clojure.set/map-invert d) 3))]
    (list (if (not (nil? (first calc)))
            (inc (first acc))
            (first acc))
          (if (not (nil? (second calc)))
            (inc (second acc))
            (second acc)))))

(defn get-checksum
  []
  (loop
    [acc `(0 0)
     l (map #(frequencies %) processed)]
    (if (empty? l)
      (* (first acc) (second acc))
      (recur (total-dupes (first l) acc)
             (rest l)))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn compare-letters
  [s r]
  (let [sets (map #(clojure.set/intersection (set (sort s))
                                             (set (sort %))) r)
        diff (apply str (first (map #(clojure.set/difference (set (sort s))
                                                             (set (sort %))) r)))
        x (remove empty? sets)]
    (if (empty? x)
      nil
      (if (not= (count diff) 1)
        nil
        s))))

(defn get-common-letters
  []
  (loop
    [l processed]
    (let [t (compare-letters (first l) (rest l))]
      (if (not (nil? t))
        t
        (recur (rest l))))))


