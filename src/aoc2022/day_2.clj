(ns aoc2022.day-2
  (:require [clojure.string :as str]))

(def raw
  (->> "resources/2022/day_2.txt"
       (slurp)
       (str/split-lines)))

(def scores-p1 {:AX 4 :AY 8 :AZ 3 :BX 1 :BY 5 :BZ 9 :CX 7 :CY 2 :CZ 6})

(def scores-p2 {:AX 3 :AY 4 :AZ 8 :BX 1 :BY 5 :BZ 9 :CX 2 :CY 6 :CZ 7})

(defn transform-inputs
  []
  (map #(keyword (apply str (str/split % #" "))) raw))

;PART 1 & 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn get-scores
  [score-map]
  (reduce + (map #(get score-map %) (transform-inputs))))

;(get-scores scores-p1) part-1
;(get-scores scores-p2) part-2