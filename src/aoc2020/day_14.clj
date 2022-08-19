(ns aoc2020.day_14
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2020/day_14.txt"))
(def processed (str/split-lines raw))

(defn decode-mem [line]
  (let [[addr val] (map #(Long/parseLong %) (re-seq #"\d+" line))]
    (list addr val)))

(defn parse-lines [lines]
  (reverse (reduce (fn [lst line]
                     (let [[op arg] (str/split line #" = ")]
                       (case (apply str (take 3 op))
                         "mas" (cons (list :mask (vec arg)) lst)
                         "mem" (cons (decode-mem line) lst))))
                   () lines)))