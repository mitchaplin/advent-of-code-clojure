(ns aoc2022.day-17)

(def raw)

;; place, using current pointer

;; move into bottom pos
;; remove full lines, add 1

;[[0 0] [0 1] [1 0] [2 2] [3 3] [1 1] [3 1] [3 1]]
(defn remove-row-update-points
  [points score]
  (let [raw-points (map set (map second (group-by second points)))
        raw-pc (count raw-points)
        removed-points (remove #(= (count %) 4) raw-points)
        removed-pc (count removed-points)
        next-points (apply concat removed-points)
        score (+ score (- raw-pc removed-pc))]
    [next-points score]))
