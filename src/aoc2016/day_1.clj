(ns aoc2016.day_1
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2016/day_1.txt"))
(def processed (str/split (str/trim-newline raw) #", "))

(def processed-data (map #(conj [] (str (first  %)) (str/join (rest %))) processed))
(def test-data (map #(conj [] (str (first  %)) (str/join (rest %))) ["R8" "R4" "R4" "R8"]))

(defn determine-position
  [position amount dir]
  (case dir
    "D" [(first position) (+ (second position) (read-string amount))]
    "U" [(first position) (- (second position) (read-string amount))]
    "L" [(- (first position) (read-string amount)) (second position)]
    "R" [(+ (first position) (read-string amount)) (second position)]))

(defn determine-ranges
  [iter position dir]
  (case dir
    "D" [(first position) (+ (second position) iter)]
    "U" [(first position) (- (second position) iter)]
    "L" [(- (first position) iter) (second position)]
    "R" [(+ (first position) iter) (second position)]))

(defn calculate-new-direction
  [dir new-inst]
  (case new-inst
    "R" (if (= dir "U") "R" (if (= dir "R") "D" (if (= dir "D") "L" (if (= dir "L") "U"))))
    "L" (if (= dir "U") "L" (if (= dir "L") "D" (if (= dir "D") "R" (if (= dir "R") "U"))))))

(defn process-instructions-1
  [data]
  (loop [instructions data dir "U" position [0 0]]
    (if (empty? instructions)
      (+ (Math/abs (first position)) (Math/abs (second position)))
      (let [new-instructions (rest instructions) new-dir (calculate-new-direction dir (ffirst instructions))]
        (recur new-instructions new-dir (determine-position position (second (first instructions)) new-dir))))))

;; Part 2
(defn process-instructions-2
  [data]
  (loop
    [instructions data
     dir "U"
     position [0 0]
     position-list [[]]]
    (if (empty? instructions)
      (apply min (set (map (fn [x]  (+ (Math/abs (ffirst x))
                                       (Math/abs (second (first x)))))
                           (remove #(= (second %) 1)
                                   (frequencies (partition 2 (flatten (rest position-list))))))))
      (let [new-instructions (rest instructions)
            new-dir (calculate-new-direction dir (ffirst instructions))
            new-pos (determine-position position (second (first instructions)) new-dir)
            new-pos-range (map #(determine-ranges % position new-dir) (range 0 (read-string (second (first instructions)))))
            pos-list (conj position-list new-pos-range)]
        (recur new-instructions new-dir new-pos pos-list)))))
