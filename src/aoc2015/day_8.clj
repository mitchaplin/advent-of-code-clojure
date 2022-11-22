(ns aoc2015.day-8
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2015/day_8.txt"))
(def processed (str/split-lines raw))

(defn calculate-total-characters
  [s]
  (count (str/split s #"")))

(defn aggregate-totals
  []
  (reduce + (map calculate-total-characters processed)))

(defn hexify [s]
  (apply str (map #(format "%02x" %) (.getBytes s "UTF-8"))))

(defn unhexify [s]
  (let [bytes (into-array Byte/TYPE
                          (map (fn [[x y]]
                                 (unchecked-byte (Integer/parseInt (str x y) 16)))
                               (partition 2 s)))]
    (String. bytes "UTF-8")))

(defn calculate-string-literals
  [s])

(defn aggregate-string-literal-totals
  []
  (reduce + (map calculate-string-literals processed)))

(defn part-1
  []
  (- (aggregate-totals) (aggregate-string-literal-totals)))
