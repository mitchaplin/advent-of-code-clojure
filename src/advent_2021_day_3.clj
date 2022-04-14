(ns advent_2021_day_3
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2021/day_3.txt"))
(def processed (map #(str/split % #" ") (str/split-lines raw)))
(def flat-processed (apply concat (map #(str/split % #" ") (str/split-lines raw))))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn pluck-value
  [v f]
  (if (f (val (first v)) (val (second v)))
    (ffirst v)
    (first (second v))))

(defn p1
  []
  (loop [p processed
         tot 0
         acc []]
    (if (<= (count (ffirst processed)) tot)
      (* (Long/parseLong (apply str (map #(pluck-value % <) acc)) 2)
         (Long/parseLong (apply str (map #(pluck-value % >) acc)) 2))
      (recur
        p
        (inc tot)
        (conj
          acc
          (frequencies
            (map #(nth (first %) tot) p)))))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn pluck-value-deterministic
  [v f]
  (if (and (= f <)
           (= (val (first v)) (val (second v))))
    \0
    (if (and (= f >)
             (= (val (first v)) (val (second v))))
      \1
      (if (f (val (first v)) (val (second v)))
        (ffirst v)
        (first (second v))))))

(defn filtered-results
  [iter value f p]
  (filter #(= (pluck-value-deterministic value f) (nth % iter)) p))

(defn p2
  [g]
  (loop [p flat-processed
         tot 0]
    (if (= (count p) 1)
      p
      (recur
        (filtered-results tot
                          (frequencies
                            (map #(nth % tot) p))
                          g
                          p)
        (inc tot)))))

(defn compute-result
  []
  (*
    (Long/parseLong (first (p2 >)) 2)
    (Long/parseLong (first (p2 <)) 2)))