(ns advent_2017_day_6
  (:require [clojure.string :as str]))

(def test-input [0 2 7 0])

(def input [10 3 15 10 5 15 5 15 9 2 5 8 5 2 3 6])

(defn calculate-start
  [start state]
  (if (>= (inc start) (count state))
    0
    (inc start)))

(defn redistribute
  [curr-state]
  (let [max-num (apply max curr-state)
        starting (.indexOf curr-state max-num)
        cleared (update curr-state
                        starting
                        #(Integer/parseInt "0" %))]
    (loop
      [num max-num
       s starting
       new-state cleared]
      (let [new-start (calculate-start
                        s
                        new-state)]
        (if (zero? num)
          new-state
          (recur (dec num)
                 new-start
                 (update new-state
                         new-start
                         inc)))))))

(defn find-redistribution-cycles
  []
  (loop
    [input input
     seen []]
    (let [current-state (redistribute input)
          updated-seen (conj seen current-state)]
      (if (true? (some (partial = current-state) seen))
        (count updated-seen)
        (recur current-state
               updated-seen)))))

(defn get-distance-between
  [state curr]
  (let [first-index (.indexOf state curr)
        cleared-first (update state first-index #(replace % nil))
        second-index (.indexOf cleared-first curr)]
    (- second-index first-index)))

(defn find-redistribution-cycles-2
  []
  (loop
    [input input
     seen []]
    (let [current-state (redistribute input)
          updated-seen (conj seen current-state)]
      (if (true? (some (partial = current-state) seen))
        (get-distance-between updated-seen current-state)
        (recur current-state
               updated-seen)))))

