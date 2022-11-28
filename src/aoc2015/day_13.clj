(ns aoc2015.day-13
  (:require [clojure.string :as str]
            [utils :as utils]
            [clojure.math.combinatorics :as combo]))

(def raw (slurp "resources/2015/day_13_2.txt"))
(def processed (str/split-lines raw))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defn pluck-keys
  [l]
  (let [[from _ _ _ _ _ _ _ _ _ to] (str/split l #" ")
        trimmed-to (apply str (butlast to))]
    (map keyword (set (list from trimmed-to)))))

(def map-keys
  (set (apply concat (map pluck-keys processed))))

(defn update-graph
  [g [from _ fnc amt _ _ _ _ _ _ to]]
  (let [f (if (= fnc "gain") + -)
        trimmed-to (apply str (butlast to))]
    (update-in g [(keyword from)] assoc (keyword trimmed-to) (f 0 (utils/parse-int amt)))))

(def cost-graph
  (loop [i processed
         g {}]
    (if (empty? i)
      g
      (recur (rest i)
             (update-graph g (str/split (first i) #" "))))))

(def pairs
  (->>
    (combo/permutations map-keys)
    (map vec)
    (map #(conj % (first %)))
    (map #(partition 2 1 %))))

(defn calc-new-totals
  [p]
  (reduce + (map (fn [[x y]] (+ (get (get cost-graph x) y)
                                (get (get cost-graph y) x))) p)))

(defn part-1
  []
  (loop [p pairs
         totals []]
    (if (empty? p)
      (apply max totals)
      (recur (rest p)
             (conj totals (calc-new-totals (first p)))))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Or run p1 on second input

(def map-keys-including-me
  (set (apply concat (conj (map pluck-keys processed) [:Me]))))

(def update-cost-graph-with-me
  (into {} (map #(vector (keyword (first %)) (assoc (second %) :Me 0)) cost-graph)))

(def cost-graph-including-me (assoc
                               update-cost-graph-with-me
                               :Me
                               (apply hash-map (vec (interleave
                                                      (mapv identity map-keys)
                                                      (repeat (count map-keys-including-me) 0))))))

(defn calc-new-totals-2
  [p]
  (reduce + (map (fn [[x y]] (+ (get (get cost-graph-including-me x) y)
                                (get (get cost-graph-including-me y) x))) p)))

(def pairs-2
  (->>
    (combo/permutations map-keys-including-me)
    (map vec)
    (map #(conj % (first %)))
    (map #(partition 2 1 %))))

(defn part-2
  []
  (loop [p pairs-2
         totals []]
    (if (empty? p)
      (apply max totals)
      (recur (rest p)
             (conj totals (calc-new-totals-2 (first p)))))))