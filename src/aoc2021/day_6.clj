(ns aoc2021.day_6
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2021/day_6.txt"))
(def processed (map #(Integer/parseInt %) (str/split raw #",")))


(def practice-initial-state [3 4 3 1 2])
(def initial-ages {:0 0 :1 0 :2 0 :3 0 :4 0 :5 0 :6 0 :7 0 :8 0})

(defn initialize-ages
  [initial fish-dir]
  (loop
    [i initial
     fd fish-dir]
    (if (empty? i)
      fd
      (recur (rest i)
             (update fd (keyword (str (first i))) inc)))))

(defn update-vals
  [fish-dir counter initial]
  (cond
    (= counter 8)
    (assoc fish-dir :8 (get initial :0) :7 (get initial :8))

    (= counter 6)
    (assoc fish-dir :6 (+ (get initial :0) (get initial :7)) :5 (get initial :6))

    (= counter 0)
    (assoc fish-dir :0 (get initial :1))

    :else
    (assoc fish-dir (keyword (str (dec counter)))
                    (get initial (keyword (str counter))))))

(defn update-counts
  [fish-dir]
  (loop
    [fd fish-dir
     counter 8]
    (if (neg? counter)
      fd
      (recur (update-vals fd counter fish-dir)
             (dec counter)))))

(defn spawn-fish
  [total]
  (loop
    [iter 0
     fd (initialize-ages processed initial-ages)]
    (if (>= iter total)
      (reduce + (vals fd))
      (recur (inc iter)
             (update-counts fd)))))