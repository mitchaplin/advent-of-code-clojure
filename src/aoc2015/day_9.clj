(ns aoc2015.day-9
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def raw (slurp "resources/2015/day_9.txt"))
(def processed (str/split-lines raw))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defn pluck-keys
  [l]
  (let [[from _ to _ _] (map #(str/split % #" ") l)]
    (list from to)))

(defn pluck-vals
  [l]
  (let [[_ _ _ _ values] (map #(str/split % #" ") l)]
    (utils/parse-int (first values))))

(defn map-key-vals
  []
  (let [k (map #(str/split % #" ") processed)
        x (map keyword (set (flatten (map pluck-keys k))))
        y (flatten (map pluck-vals k))]
    (list x y)))

(def combos
  (combo/permutations (first (map-key-vals))))

(defn update-graph
  [[from _ to _ dist]]
  [(set [(keyword from) (keyword to)]) (utils/parse-int dist)])

(def make-cost-graph
  (loop [i processed
         g []]
    (if (empty? i)
      (into {} g)
      (recur (rest i)
             (conj g  (update-graph (str/split (first i) #" ")))))))

(defn lookup-step-cost
  [line]
  (loop [l (rest line)
         start (first line)
         total 0]
    (if (empty? l)
      total
      (recur (rest l)
             (first l)
             (+ total (get make-cost-graph #{start (first l)}))))))

(defn part-1
  []
  (apply min (map lookup-step-cost combos)))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn part-2
  []
  (apply max (map lookup-step-cost combos)))