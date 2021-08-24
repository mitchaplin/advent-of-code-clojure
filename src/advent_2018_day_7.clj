(ns advent_2018_day_7
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2018/day_7.txt"))
(def processed (str/split-lines raw))

(defn line-to-pair
  [line]
  (rest (re-find #"Step (\w) must be finished before step (\w)" line)))

(defn lines-to-pairs
  [lines]
  (map line-to-pair lines))

(defn setup-graph
  [pairs]
  (reduce (fn [graph key]
            (assoc graph key #{}))
          {} (distinct (flatten pairs))))

(defn create-graph
  [pairs]
  (reduce (fn [ret [dep key]]
            (update ret key conj dep))
          (setup-graph pairs) pairs))

(defn remove-step
  [graph step]
  (let [graph (dissoc graph step)]
    (reduce (fn [ret key]
              (update ret key disj step))
            graph (keys graph))))

(defn find-next-step
  [graph]
  (let [sortable (sort #(compare (first %1) (first %2))
                       (map #(list (first %) (count (last %))) graph))]
    (ffirst (sort #(compare (last %1) (last %2)) sortable))))

(defn find-step-order
  [graph]
  (loop [g graph steps ()]
    (cond
      (empty? g) (apply str (reverse steps))
      :else
      (let [step (find-next-step g)]
        (recur (remove-step g step) (cons step steps))))))

(defn p01
  []
  (->> processed
       (lines-to-pairs)
       (create-graph)
       (find-step-order)))