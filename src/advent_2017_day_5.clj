(ns advent_2017_day_5
  (:require [clojure.string :as str]))

(def raw
  (->> "resources/2017/day_5.txt"
       (slurp)
       (str/split-lines)
       (map #(Integer/parseInt %))
       (apply vector)))

(defn update-board-1
  [b p]
  (let [next (get b p)]
    (if (nil? next) (list "break" b)
                    (list (+ next p)
                          (update b p inc)))))

(defn get-next-offset
  [p]
  (if (<= 3 p) dec inc))

(defn update-board-2
  [b p]
  (let [next (get b p)]
    (if (nil? next) (list "break" b)
                    (list (+ next p)
                          (update b p (get-next-offset next))))))

(defn escape
  []
  (loop
      [lst raw
       pointer 0
       steps 0]
      (let [[p b] (update-board-2 lst pointer)]
        (if (= p "break")
          steps
          (recur b
                 p
                 (inc steps))))))

