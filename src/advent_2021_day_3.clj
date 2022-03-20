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

;(def raw (slurp "resources/2021/day_3_practice.txt"))
;(def processed (str/split-lines raw))
;
;(defn collect-items
;  [idx]
;  (map #(get % idx) processed))
;
;(defn start-stuff
;  []
;  (loop [idx 0
;         new-coll []]
;    (if (< 4 idx)
;      new-coll
;      (recur (inc idx)
;             (conj new-coll (collect-items idx))))))
;
;(defn testtest
;  []
;  (* (Integer/parseInt (apply str (map first (map #(if (< (second (second %)) (second (first %))) (first %) (second %)) (start-stuff)))) 2)
;     (Integer/parseInt (apply str (map first (map #(if (> (second (second %)) (second (first %))) (first %) (second %)) (start-stuff)))) 2)))
;
;;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;(defn collect-items-2
;  [idx x]
;  (map #(get % idx) x))
;
;(defn start-stuff
;  [x]
;  (loop [idx 1
;         new-coll []
;         t x]
;    (if (< 4 idx)
;      new-coll
;      (recur (inc idx)
;             (conj new-coll (collect-items-2 idx t))
;             t))))
;
;(defn determine-co2-rating
;  [t]
;  (first (map #(if (> (second (second (frequencies %))) (second (first (frequencies %)))) 1 0) (start-stuff t))))
;
;(defn xyz
;  [p]
;  (remove empty? (map #(if (= (Integer/parseInt (str (first %))) (determine-co2-rating p)) % "") p)))