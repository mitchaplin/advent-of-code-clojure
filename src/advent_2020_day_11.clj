(ns advent_2020_day_11
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2020/day_11_practice.txt"))
(def processed (str/split-lines raw))

(def max-len (count (first processed)))
(def max-size (count processed))

(defn check-oob
  [x y]
  (list (> (inc x) max-len)
        (< (dec x) 0)
        (> (inc y) max-size)
        (< (dec y) 0)))

(defn update-board-snapshot
  [old x y new]
  (if (and (= x max-len)
           (= y max-size))
    new
    old))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn check-occ-satellite-seats
  [x y board]
  (let [[px nx py ny] (check-oob x y)]
    (+ (if (= \# (get (get board (inc y)) x)) 1 0)
       (if (= \# (get (get board (dec y)) x)) 1 0)
       (if (= \# (get (get board y) (inc x))) 1 0)
       (if (= \# (get (get board y) (dec x))) 1 0)
       (if (= \# (get (get board (inc y)) (inc x))) 1 0)
       (if (= \# (get (get board (dec y)) (dec x))) 1 0)
       (if (= \# (get (get board (inc y)) (dec x))) 1 0)
       (if (= \# (get (get board (dec y)) (inc x))) 1 0))))


(defn apply-rules
  [x y board snap]
  (let [seat-total (check-occ-satellite-seats x y snap)
        row (get board y)
        seat (get (get board y) x)]
    (cond (= seat \.)
          board

          (and (zero? seat-total) (= seat \L))
          (update board y
                  (fn [_] (str (subs row 0 x)
                               \#
                               (subs row (inc x)))))

          (and (>= seat-total 4) (= seat \#))
          (update board y
                  (fn [_] (str (subs row 0 x)
                               \L
                               (subs row (inc x)))))

          :else
          board)))

(defn mutate-board
  [initial start-x start-y]
  (loop
    [x start-x
     y start-y
     updated-board initial
     board-snapshot initial]
    (let [[px nx py ny] (check-oob x y)]

      (if (and (= x max-len)
               (= y max-size))
        updated-board
        (cond px
              (recur 0 (if py 0 (inc y))
                     (apply-rules x y updated-board board-snapshot)
                     (update-board-snapshot board-snapshot x y updated-board))

              :else
              (recur (inc x)
                     y
                     (apply-rules x y updated-board board-snapshot)
                     (update-board-snapshot board-snapshot x y updated-board)))))))

(defn compare-results
  [init sx sy]
  (let [new-snap (mutate-board init sx sy)]
    (if (= init new-snap)
      (reduce + (map count (map #(re-seq #"#" %) new-snap)))
      (compare-results new-snap 0 0))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn check-occ-satellite-seats-2
  [x y board]

  (+
    ;down y x
    (loop [acc 0
           newx x
           newy (inc y)]
      (if (or (> newy max-size) (> acc 0) (and (= \L (get (get board newy) newx)) (zero? acc)))
        (if (> acc 0) 1 0)
        (recur (+ acc (if (= \# (get (get board newy) newx)) 1 0))
               newx
               (inc newy))))

    ;up y x
    (loop [acc 0
           newx x
           newy (dec y)]
      (if (or (< newy 0) (> acc 0) (and (= \L (get (get board newy) newx)) (zero? acc)))
        (if (> acc 0) 1 0)
        (recur (+ acc (if (= \# (get (get board newy) newx)) 1 0))
               newx
               (dec newy))))

    ;y right x
    (loop [acc 0
           newx (inc x)
           newy y]
      (if (or (> newx max-size) (> acc 0) (and (= \L (get (get board newy) newx)) (zero? acc)))
        (if (> acc 0) 1 0)
        (recur (+ acc (if (= \# (get (get board newy) newx)) 1 0))
               (inc newx)
               newy)))

    ;y left x
    (loop [acc 0
           newx (dec x)
           newy y]
      (if (or (< newx 0) (> acc 0) (and (= \L (get (get board newy) newx)) (zero? acc)))
        (if (> acc 0) 1 0)
        (recur (+ acc (if (= \# (get (get board newy) newx)) 1 0))
               (dec newx)
               newy)))

    ;down y right x
    (loop [acc 0
           newx (inc x)
           newy (inc y)]
      (if (or (or (> newy max-size) (> newx max-len)) (> acc 0) (and (= \L (get (get board newy) newx)) (zero? acc)))
        (if (> acc 0) 1 0)
        (recur (+ acc (if (= \# (get (get board newy) newx)) 1 0))
               (inc newx)
               (inc newy))))

    ;up y left x
    (loop [acc 0
           newx (dec x)
           newy (dec y)]
      (if (or (or (< newy 0) (< newx 0)) (> acc 0) (and (= \L (get (get board newy) newx)) (zero? acc)))
        (if (> acc 0) 1 0)
        (recur (+ acc (if (= \# (get (get board newy) newx)) 1 0))
               (dec newx)
               (dec newy))))

    ;down y left x
    (loop [acc 0
           newx (dec x)
           newy (inc y)]
      (if (or (or (> newy max-size) (< newx 0)) (> acc 0) (and (= \L (get (get board newy) newx)) (zero? acc)))
        (if (> acc 0) 1 0)
        (recur (+ acc (if (= \# (get (get board newy) newx)) 1 0))
               (dec newx)
               (inc newy))))

    ;up y right x
    (loop [acc 0
           newx (inc x)
           newy (dec y)]
      (if (or (or (< newy 0) (> newx max-len)) (> acc 0) (and (= \L (get (get board newy) newx)) (zero? acc)))
        (if (> acc 0) 1 0)
        (recur (+ acc (if (= \# (get (get board newy) newx)) 1 0))
               (inc newx)
               (dec newy))))))

(defn apply-rules-2
  [x y board snap]
  (let [seat-total (check-occ-satellite-seats-2 x y snap)
        row (get board y)
        seat (get (get board y) x)]
    (cond (= seat \.)
          board

          (and (zero? seat-total) (= seat \L))
          (update board y
                  (fn [_] (str (subs row 0 x)
                               \#
                               (subs row (inc x)))))

          (and (>= seat-total 5) (= seat \#))
          (update board y
                  (fn [_] (str (subs row 0 x)
                               \L
                               (subs row (inc x)))))

          :else
          board)))

(defn mutate-board-2
  [initial start-x start-y]
  (loop
    [x start-x
     y start-y
     updated-board initial
     board-snapshot initial]
    (let [[px nx py ny] (check-oob x y)]

      (if (and (= x max-len)
               (= y max-size))
        updated-board
        (cond px
              (recur 0 (if py 0 (inc y))
                     (apply-rules-2 x y updated-board board-snapshot)
                     (update-board-snapshot board-snapshot x y updated-board))

              :else
              (recur (inc x)
                     y
                     (apply-rules-2 x y updated-board board-snapshot)
                     (update-board-snapshot board-snapshot x y updated-board)))))))

(defn compare-results-2
  [init sx sy]
  (let [new-snap (mutate-board-2 init sx sy)]
    (println new-snap (= init new-snap))
    (if (= init new-snap)
      (reduce + (map count (map #(re-seq #"#" %) new-snap)))
      (compare-results-2 new-snap 0 0))))

