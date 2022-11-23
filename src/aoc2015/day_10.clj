(ns aoc2015.day-10)

(def inp [1 1 1 3 2 2 2 1 1 3])

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defn look-and-say
  [s]
  (->> s
       (partition-by identity)
       (mapcat #(list (count %) (first %)))))

(defn part-1
  []
  (count (reduce (fn [acc _] (look-and-say acc)) inp (range 0 40))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defn part-2
  []
  (count (reduce (fn [acc _] (look-and-say acc)) inp (range 0 50))))