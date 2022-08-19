(ns aoc2020.day_12
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2020/day_12.txt"))
(def processed (str/split-lines raw))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn update-pointer
  [pointer inst dist dir]
  (cond (= inst "F")
        (cond (= dir 0)
              (list (+ dist (first pointer)) (second pointer) dir)
              (= dir 1)
              (list (first pointer) (- (second pointer) dist) dir)
              (= dir 2)
              (list (- (first pointer) dist) (second pointer) dir)
              (= dir 3)
              (list (first pointer) (+ dist (second pointer)) dir))
        (= inst "N")
        (list (first pointer) (+ dist (second pointer)) dir)
        (= inst "S")
        (list (first pointer) (- (second pointer) dist) dir)
        (= inst "E")
        (list (+ dist (first pointer)) (second pointer) dir)
        (= inst "W")
        (list (- (first pointer) dist) (second pointer) dir)
        :else
        (list (first pointer) (second pointer) dir)))

(defn transform-degrees
  [dist dir]
  (cond (= dist 90)
        1
        (= dist 180)
        2
        (= dist 270)
        3
        :else
        dir))

(defn check-neg
  [dist dir]
  (let [n (- dir (transform-degrees dist dir))]
    (if (neg? n)
      (Math/abs (- 4 (Math/abs n)))
      n)))

(defn check-g4
  [dist dir]
  (let [n (+ (transform-degrees dist dir) dir)]
    (if (> n 3)
      (Math/abs (- 4 n))
      n)))

(defn update-dir
  [inst dist dir]
  (cond (= inst "R")
        (if (or (= dist 360) (= dist 0))
          dir
          (check-g4 dist dir))
        (= inst "L")
        (if (or (= dist 360) (= dist 0))
          dir
          (check-neg dist dir))
        :else
        dir))


(defn navigate
  [data]
  (loop [d data
         pointer [0 0 0]]
    (if (empty? d)
      (+ (Math/abs (first pointer)) (Math/abs (second pointer)))
      (let [i (first d)
            [inst dist] (list (subs i 0 1) (Integer/parseInt (subs i 1)))
            new-dir (update-dir inst dist (last pointer))
            new-pointer (update-pointer pointer inst dist new-dir)]
        (recur (rest d) new-pointer)))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn update-ship
  [[ship waypoint] inst dist]
  (cond (= inst "F")
        (vector (vector (+ (first ship) (* dist (first waypoint)))
                        (+ (second ship) (* dist (second waypoint)))) waypoint)
        :else
        (list ship waypoint)))

(defn update-waypoint
  [[ship waypoint] inst dist]
  (cond
    (= inst "R")
    (cond (= dist 90)
          (list (second waypoint)
                (unchecked-negate-int (first waypoint)))
          (= dist 180)
          (list (unchecked-negate-int (first waypoint))
                (unchecked-negate-int (second waypoint)))
          (= dist 270)
          (list (unchecked-negate-int (second waypoint))
                (first waypoint))
          (= dist 360)
          waypoint
          (= dist 0)
          waypoint
          :else
          waypoint)

    (= inst "L")
    (cond (= dist 90)
          (list (unchecked-negate-int (second waypoint))
                (first waypoint))
          (= dist 180)
          (list (unchecked-negate-int (first waypoint))
                (unchecked-negate-int (second waypoint)))
          (= dist 270)
          (list (second waypoint) (unchecked-negate-int
                                    (first waypoint)))
          (= dist 360)
          waypoint
          (= dist 0)
          waypoint
          :else
          waypoint)

    (= inst "N")
    (list (first waypoint) (+ dist (second waypoint)))
    (= inst "S")
    (list (first waypoint) (- (second waypoint) dist))
    (= inst "E")
    (list (+ dist (first waypoint)) (second waypoint))
    (= inst "W")
    (list (- (first waypoint) dist) (second waypoint))
    :else
    (list (first waypoint) (second waypoint))))

(defn navigate-with-waypoint
  [data]
  (loop [d data
         binary [[0 0] [10 1]]]
    (if (empty? d)
      (+ (Math/abs (ffirst binary))
         (Math/abs (second (first binary))))
      (let [i (first d)
            [inst dist] (list (subs i 0 1) (Integer/parseInt (subs i 1)))
            new-waypoint (update-waypoint binary inst dist)
            new-binary (update-ship (list (first binary) new-waypoint) inst dist)]
        (recur (rest d) new-binary)))))