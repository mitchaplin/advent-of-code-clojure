(ns aoc2022.day-9
  (:require
    [clojure.string :as str]))

(def raw (map #(str/split % #" ") (utils/read-file "resources/2022/day_9.txt")))
(def processed (apply concat (map (fn [[x y]] (repeat (utils/parse-int y) x)) raw)))

(def raw-2 (str/split-lines (slurp "resources/2022/day_9.txt")))

(defn get-manhatten-distance
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn move-head
  [dir [x y]]
  (cond (=  dir "R")
        [(inc x) y]

        (=  dir "L")
        [(dec x) y]

        (=  dir "U")
        [x (dec y)]

        (=  dir "D")
        [x (inc y)]))

(defn move-tail
  [last-head-pos head [tx ty]]
 (cond
   (= head [tx ty])
   [tx ty]

   (= 1 (get-manhatten-distance head [tx ty]))
   [tx ty]

   (and
     (= 2 (get-manhatten-distance head [tx ty]))
     (or (= head [(dec tx) (inc ty)])
         (= head [(inc tx) (inc ty)])
         (= head [(inc tx) (dec ty)])
         (= head [(dec tx) (dec ty)])))
   [tx ty]

   (>= (get-manhatten-distance head [tx ty]) 2)
   last-head-pos

   :else
   last-head-pos))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn part-1
  []
  (loop [insts processed
         visited []
         head [0 0]
         tail [0 0]]
    (if (empty? insts)
      (count (set (conj visited (move-tail head head tail))))
      (let [new-head (move-head (first insts) head)
             new-tail (move-tail head new-head tail)]
         (recur (rest insts)
                (conj visited tail)
                new-head
                new-tail)))))

;(part-1)

;PART 2  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn generate-initial-snake
  [knots]
  (repeat knots {:current-head [0 0] :visited [[0 0]]}))

(defn get-move-value
  [dir]
  (cond (=  dir "R")
        [0 1]

        (=  dir "L")
        [0 -1]

        (=  dir "U")
        [1 0]

        (=  dir "D")
        [-1 0]

        :else
        [0 0]))

(defn calculate-new-move
  [[x1 x2] [y1 y2]]
  (cond
    (< (utils/get-max-manhatten-distance x1 x2 y1 y2) 1)
    [0 0]

    (= x1 y1)
    [0 (/ (- x2 y2) 2)]

    (= x2 y2)
    [(/ (- x1 y1) 2) 0]

    (and (< y2 x2)
         (< y1 x1))
    [1 1]

    (and (< y2 x2)
         (< x1 y1))
    [-1 1]

    (and (< x2 y2)
         (< x1 y1))
    [-1 -1]

    (and (< x2 y2)
         (< y1 x1))
    [1 -1]

    :else
    nil))

(defn update-snake-positions
  [snake move]
  (let [[head & tail] snake
        current-head-position  (:current-head head)
        new-head-position  (vec (map + move current-head-position))
        visited (:visited head)
        new-visited (conj visited new-head-position)
        next-chunk  {:current-head new-head-position :visited new-visited}]
    (if (nil? tail)
      (list next-chunk)
      (let [new-move (calculate-new-move new-head-position (:current-head (first tail)))]
        (cons next-chunk (update-snake-positions tail new-move))))))

(defn execute-move
  [starting-snake inst]
  (let [[direction amt] (str/split inst #" ")
        move (get-move-value direction)]
    (loop [snake starting-snake
           move-amount (utils/parse-int amt)]
      (if (zero? move-amount)
        snake
        (recur (update-snake-positions snake move) (dec move-amount))))))

(defn run-instructions-2
  [num-knots instructions]
  (let [starting-snake (generate-initial-snake num-knots)]
    (reduce execute-move starting-snake instructions)))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn part-2
  [knots]
  (->> raw-2
       (run-instructions-2 knots)
       last
       vals
       last
       set
       count))

;(part-2 10)

