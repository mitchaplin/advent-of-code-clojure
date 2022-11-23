(ns aoc2015.day-8
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2015/day_8.txt"))
(def processed (str/split-lines raw))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defn mutate-string
  [s]
  (-> s
      (str/replace #"\\x[0-9a-fA-F][0-9a-fA-F]" "~")
      (str/replace #"\\\\" "~")
      (str/replace #"\\\"" "~")))

(defn calculate-total-characters
  [s]
  (count (str/split s #"")))

(defn aggregate-totals
  []
  (reduce + (map calculate-total-characters processed)))

(defn calculate-string-literals
  [s]
  (let [replaced-hex (mutate-string s)
        total (- (count (str/split replaced-hex #"")) 2)
        _ (println total replaced-hex s)]
    total))

(defn aggregate-string-literal-totals
  []
  (reduce + (map calculate-string-literals processed)))

(defn part-1
  []
  (- (aggregate-totals) (aggregate-string-literal-totals)))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defn part-2
  []
  (- (reduce + (map count (map pr-str processed))) (aggregate-totals)))