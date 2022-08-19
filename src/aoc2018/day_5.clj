(ns aoc2018.day_5
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def raw (slurp "resources/2018/day_5.txt"))

(defn transform
  [p]
  (loop [[c & p] p
         result []]
    (let [r (first result)]
      (cond
        (nil? c) (apply str (reverse result))
        (nil? r) (recur p (cons c result))
        (and (not= c r)
             (= (str/upper-case c)
                (str/upper-case r))) (recur p (rest result))
        :else (recur p (cons c result))))))

(defn derive-combos
  []
  (map str (->> (range (int \a) (inc (int \z))) (map char))))

(def combos (derive-combos))

(defn apply-combos-to-polymers
  [c]
  (apply str (remove #(= (str/lower-case %) c) (into [] raw))))

(defn idk
  []
  (set (map apply-combos-to-polymers combos)))

(defn react-polymer
  [p]
  (->> p
       (transform)
       (count)))

(defn react-polymer-p2
  []
  (apply min (map react-polymer (idk))))