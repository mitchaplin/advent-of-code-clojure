(ns aoc2022.day-6)

(def raw (first (utils/read-file "resources/2022/day_6.txt")))

(defn find-signal
  [i part-val comp-val op]
  (loop [t (map set (partition part-val 1 i))
         counter 0]
    (if (op (count (first t)) comp-val)
      (+ part-val counter)
      (recur (rest t)
             (inc counter)))))

;(find-signal raw 4 3 >) part-1
;(find-signal raw 14 14 =) part-2