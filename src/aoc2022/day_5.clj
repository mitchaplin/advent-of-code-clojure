(ns aoc2022.day-5
  (:require [clojure.string :as str]))

(def raw (utils/read-file "resources/2022/day_5.txt"))
(def processed (map #(str/split % #" ") raw))
(def practice {:1 ["Z" "N"], :2 ["M" "C" "D"], :3 ["P"]})
(def real-data {:1 ["W" "M" "L" "F"], :2 ["B" "Z" "V" "M" "F"], :3 ["H" "V" "R" "S" "L" "Q"]
                :4 ["F" "S" "V" "Q" "P" "M" "T" "J"], :5 ["L" "S" "W"], :6 ["F" "V" "P" "M" "R" "J" "W"]
                :7 ["J" "Q" "C" "P" "N" "R" "F"], :8 ["V" "H" "P" "S" "Z" "W" "R" "B"], :9 ["B" "M" "J" "C" "G" "H" "Z" "W"]})

(defn update-cargo
  [cargo [_ amt _ from _ to] r]
  (let [f (keyword from)
        t (keyword to)
        a (utils/parse-int amt)
        updated-move (assoc cargo
                       t
                       (into [] (flatten (conj (get cargo t)
                                               (r (take-last a (get cargo f)))))))
        updated-remove (assoc updated-move
                         f
                         (into [] (drop-last a (get updated-move f))))]

    updated-remove))


;PART 1 & 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn process-instructions
  [f]
  (let []
    (loop [i processed
           c real-data]
      (if (empty? i)
       (apply str (map last (vals (into (sorted-map) c))))
       (recur (rest i)
              (update-cargo c (first i) f))))))

;(process-instructions reverse) part-1
;(process-instructions identity) part-2

