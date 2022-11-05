(ns aoc2018.day_4
  (:require [clojure.string :as str]
            [utils :as utils]))

(def raw (slurp "resources/2018/day_4.txt"))
(def processed (sort (map #(conj [] (subs % 1 17) (subs % 19)) (str/split-lines raw))))

(defn get-current-guard
  [cg line]
  (let [parsed (str/split (second line) #" ")]
    (if (utils/in? parsed "Guard") (apply str (rest (second parsed))) cg)))

(defn calculate-next-instruction
  [cg line timesheet]
  (let [parsed (str/split (second line) #" ")
        time (Integer/parseInt (second (str/split (first line) #":")))
        action (first parsed)]
    (cond (= action "falls")
          (conj timesheet [cg time])

          (= action "wakes")
          (apply merge timesheet (map #(list cg %) (rest (range (last (last timesheet)) time))))

          :else
          timesheet)))

(defn build-timesheet
  [processed]
  (loop
    [instructions processed
     current-guard "2459"
     timesheet (calculate-next-instruction current-guard (first instructions) [])]
    (if (empty? instructions)
      timesheet
        (recur (rest instructions)
               (get-current-guard current-guard (first instructions))
               (calculate-next-instruction current-guard (first instructions) timesheet)))))

(def full-timesheet (build-timesheet processed))
(def result (first (last (sort-by second (frequencies (map first full-timesheet))))))
(def result2 (last (sort-by val (frequencies (filter #(= (first %) result) full-timesheet)))))

(defn p1
  []
  (* (Integer/parseInt (ffirst result2)) (second (first result2))))

(defn p2
  []
  (let [[a b] (first (last (sort-by second (frequencies (map reverse full-timesheet)))))]
    (* a (Integer/parseInt b))))

