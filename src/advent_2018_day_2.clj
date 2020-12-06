(ns advent-2018-day-2
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(def practice
  `("nfghij" "abcde" "nklmno" "npqrst" "nfguij" "naxcye" "nwvxyz"))

(def raw (slurp "resources/2018/day_2.txt"))
(def processed (map #(str/split raw #"\n")))

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
  (remove empty? (flatten
                   (map #(remove nil?
                                 (cond (= 1 (Math/abs (compare (apply str (sort s))
                                                               (apply str (sort %)))))
                                       (list s %))) r))))

(defn get-common-letters
  []
  (loop
    [acc `()
     l practice]
    (if (not (empty? l))
      (apply str (clojure.set/intersection (set (first acc)) (set (second acc))))
      (recur (compare-letters (first l) (rest l)) (rest l)))))


