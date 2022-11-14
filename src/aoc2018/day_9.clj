(ns aoc2018.day_9
  (:require [clojure.string :as str])
  (:require [utils :as utils]))

(def raw (slurp "resources/2018/day_9_practice.txt"))
(def processed (str/split-lines raw))

(defn generate-scorecard
  [player-total]
  (->> (range 1 player-total)
       (map #(str (str %)))))

(defn apply-defaults
  [num-players]
  (zipmap (map keyword (generate-scorecard num-players)) (vec (repeat num-players 0))))

(defn cycle-players
  [current-player player-count]
  (if (>= current-player (dec player-count))
    1
    (inc current-player)))

(defn update-winner
  [current-idx marble-number current-board player-number score-card]
  ;(println current-idx marble-number current-board player-number score-card)
  (let [idx (dec (mod (+ (- (count current-board) 7) current-idx) (count current-board)))
        previous (current-board idx)
        new-board (vec (concat (subvec current-board 0 idx)
                               (subvec current-board (inc idx))))]
    (list
      new-board
      (-> score-card
          (update (keyword (str player-number)) + marble-number previous))
      idx)))

(defn place-marble
  [marble-number current-idx current-board player-number score-card]
  (if (utils/multiple? marble-number 23)
    (update-winner current-idx marble-number current-board player-number score-card)
    (let [new-idx (mod (+ (count current-board) (inc current-idx)) (count current-board))
          new-board (vec (concat (subvec current-board 0 new-idx)
                                 [marble-number]
                                 (subvec current-board new-idx)))]
      (list new-board score-card new-idx))))

(defn run-game
  [num-players total-count]
  (loop [score-board [0 2 1]
         score-card (apply-defaults num-players)
         current-player 4
         marble-number 3
         current-idx 2]
    (let
      [[updated-board updated-score-card new-idx] (place-marble marble-number current-idx score-board current-player score-card)]
      (if (<= total-count marble-number)
        (apply max (vals score-card))
        (recur
          updated-board
          updated-score-card
          (cycle-players current-player num-players)
          (inc marble-number)
          (inc new-idx))))))

