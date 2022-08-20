(ns aoc2018.day_4
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2018/day_4.txt"))
(def processed (sort (map #(conj [] (subs % 1 17) (subs % 19)) (str/split-lines raw))))

(defn get-current-guard
  [cg line]
  (let [parsed (str/split (second line) #" ")]
    (if (utils/in? parsed "Guard") (apply str (rest (second parsed))) cg)))

;; ["1518-11-01 00:00" "Guard #10 begins shift"]
;; ["1518-11-01 00:05" "falls asleep"]
;; ["1518-11-05 00:55" "wakes up"]

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

;"10"

(defn p1
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

(def result (ffirst (second (first (group-by second (map #(vector (ffirst %) (count %)) (map second (group-by first (p1 processed)))))))))
(def result2 (last (sort-by val  (frequencies (filter #(= (first %) result) (p1 processed))))))
(def answer (* (Integer/parseInt (ffirst result2)) (second (first result2))))
(println answer)


