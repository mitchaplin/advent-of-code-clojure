(ns advent_2020_day_6
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2020/day_6.txt"))
(def processed (str/split raw #"\n\n"))
(def formatted (map #(str/split % #" ")
                    (map #(clojure.string/replace % #"\n" " ")
                         processed)))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn calc-responses
  []
  (reduce + (map count
                 (map set
                      (map #(apply str %)
                           formatted)))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn calc-all-yes
  []
  (loop
    [p formatted
     s (map set (first p))
     acc []]
    (if (empty? p)
      (reduce + acc)
      (recur (rest p)
             (map set (first (rest p)))
             (conj acc
                   (count (apply clojure.set/intersection s)))))))