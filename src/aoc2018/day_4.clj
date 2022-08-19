(ns aoc2018.day_4
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2018/day_4_practice.txt"))
(def processed (sort (map #(conj [] (subs % 1 17) (subs % 19)) (str/split-lines raw))))

;; guard here needs to be a key
(defn update-timesheet
  [guard time timesheet]
  ;; check if key exists, if not add it
  ;;otherwise, update the existing key
  (update timesheet :123 (constantly (+ (get timesheet guard) time))))

(defn calc-time
  [current-guard time])

;; [:123 "awake"]
(defn update-guard-status
  [current-guard-status inst]
  (cond (= (first inst) \#)
        [(apply str (rest inst)) "awake"]

        (= inst "falls")
        [(first current-guard-status) "asleep"]

        (= inst "wakes")
        [(first current-guard-status) "awake"]))

;; "Guard #1663 begins shift"
(defn calculate-next-instruction
  [s]
  (let
    [i (str/split s #" ")]
    (if (= (first i) "Guard")
      (second i)
      (first i))))

(defn p1
  []
  (loop
    [current-guard-status ["2459" "awake"]
     timesheet {}
     instructions processed]
    (if (empty? instructions)
      timesheet
      (let [cg (first (keyword current-guard-status))
            instruction (first instructions)
            inst (calculate-next-instruction (second instruction))
            ngs (update-guard-status current-guard-status inst)]
        (recur ngs
              (update-timesheet cg (calc-time cg (get timesheet cg)) timesheet)
              (rest instructions))))))


