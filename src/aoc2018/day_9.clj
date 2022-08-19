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
  [current-player total-count]
  (if (= current-player total-count)
    0
    (inc current-player)))

(defn check-index-bounds
  [current-idx board]
  (let [new-idx (+ current-idx 2)
        board-total (dec (count board))
        limit (> new-idx board-total)]
    (if limit
      (- new-idx board-total)
      new-idx)))

(defn update-winner
  [current-idx marble-number current-board player-number score-card]
  (println current-idx marble-number current-board player-number score-card)
  (let [offset (- current-idx 7)
        previous (get current-board offset)
        new-board (vec (concat (subvec current-board 0 offset)
                               [marble-number]
                               (subvec current-board (inc offset))))]
    (list
      new-board
      (-> score-card (update (keyword (str player-number)) (fn [x] (+ marble-number x previous))))
      offset)))

(defn place-marble
  [marble-number current-idx current-board player-number score-card]
  (if (utils/multiple? marble-number 23)
    (update-winner current-idx marble-number current-board player-number score-card)
    (let [new-idx (check-index-bounds current-idx current-board)
          new-board (vec (concat (subvec current-board 0 new-idx)
                                 [marble-number]
                                 (subvec current-board new-idx)))]
      (list new-board score-card new-idx))))

(defn run-game
  [num-players total-count]
  (loop [score-board [0 2 1]
         score-card (apply-defaults num-players)
         current-player 4
         marble-number 4
         current-idx 1]
    (let
      [[updated-board updated-score-card new-idx] (place-marble marble-number current-idx score-board current-player score-card)
       _ (println updated-board updated-score-card new-idx)]
      (if (<= total-count marble-number)
        (apply max (vals score-card))
        (recur
          updated-board
          updated-score-card
          (cycle-players current-player total-count)
          (inc marble-number)
          new-idx)))))

