(ns aoc2015.day_5
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2015/day_5.txt"))
(def processed (str/split-lines raw))

(def naughties ["ab" "cd" "pq" "xy"])

(def vowels [\a \e \i \o \u])

(defn check-naughties
  [naughties nice-string]
  (empty? (remove false?
                  (map #(.contains nice-string %)
                       naughties))))

(defn check-repeats
  [nice-string]
  (empty? (remove false?
                  (map (fn [x] (.contains nice-string x))
                       (map #(str/join (repeat 2 %))
                            (str/split nice-string #""))))))

(defn check-vowels
  [nice-string]
  (> (reduce +
             (map #(if (.contains vowels (first %))
                     (second %) 0)
                  (frequencies nice-string))) 2))

(defn check-niceness
  [nice-string]
  (and (check-vowels nice-string) (false? (check-repeats nice-string)) (check-naughties naughties nice-string)))

(defn filter-naughties
  [processed]
  (count (remove false? (map check-niceness processed))))

;; Part 2
(defn format-new-nice-string [nice-string]
  (nthrest (reverse (partition-all 3 1 (str/split nice-string #""))) 2))

(defn find-duplicates
  [nice-string]
  (empty? (remove false?
                  (map #(and (> (second %) 1)
                             (false? (.contains nice-string
                                                (str/join (repeat 3 (ffirst %))))))
                       (frequencies (partition-all 2 1 (str/split nice-string #"")))))))

(defn determine-new-naughties
  [new-nice-string]
  (false? (empty?
            (remove false?
                    (map #(and (= (first %) (last %)) (false? (find-duplicates new-nice-string)))
                         (format-new-nice-string new-nice-string))))))

(defn filter-new-naughties
  [processed]
  (count (filter true? (map determine-new-naughties processed))))