(ns aoc2021.day-11)

(ns aoc2021.day-11
  (:require [clojure.string :as str]
            [utils :as utils]))

(def processed
  (->> (slurp "resources/2021/day_11_practice_1.txt")
       (str/split-lines)
       (map seq)
       (mapv (fn [x] (map #(utils/parse-int (str %)) x)))))

(defn update-board
  [board [x y]]
  (let [row (vec (get board y))
        _ (println x y board row)]
    (assoc board y (assoc row x (inc (get row x))))))

(defn react-satellite-points
  [board [x y]]
  (let [points-to-check (filter (fn [[x y]] (not (or (neg? x) (neg? y))))
                                (utils/exclusive-points-around [x y]))
        _ (println points-to-check)]
    (mapv #(update-board board %) points-to-check)))

;; increase all items in the board by 1,
;; keep track of all points that flash,
;; pass that into react-satellites function
(defn react-board
  []
  ())
(defn flash-lights
  []
  ())