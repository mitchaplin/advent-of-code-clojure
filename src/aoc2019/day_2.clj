(ns aoc2019.day_2
  (:require [clojure.string :as str]))

(def processed
  (->> "resources/2019/day_2.txt"
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (vec)))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defn execute-add
  [ops pc]
  (let [op1 (ops (inc pc))
        op2 (ops (+ pc 2))
        dst (ops (+ pc 3))
        sum (+ (ops op1) (ops op2))]
    (assoc ops dst sum)))

(defn execute-multiply
  [ops pc]
  (let [op1 (ops (inc pc))
        op2 (ops (+ pc 2))
        dst (ops (+ pc 3))
        prd (* (ops op1) (ops op2))]
    (assoc ops dst prd)))

(defn execute-ops
  [noun verb opcodes]
  (let [opcodes (assoc opcodes 1 noun 2 verb)]
    (loop [opcodes opcodes
           pc 0]
      (cond
        (= (opcodes pc) 99)
        (opcodes 0)

        (= (opcodes pc) 1)
        (recur (execute-add opcodes pc)
               (+ pc 4))

        (= (opcodes pc) 2)
        (recur (execute-multiply opcodes pc)
               (+ pc 4))))))

(defn part-1
  []
  (->> processed
       (execute-ops 12 2)))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn compute-result
  [v opcodes]
  (loop [noun 0
         verb 0]
    (let [result (execute-ops noun verb opcodes)]
      (cond
        (= result v) (+ (* 100 noun) verb)
        (< verb 99)  (recur noun (inc verb))
        (< noun 99)  (recur (inc noun) 0)))))

(defn part-2
  []
  (->> processed
       (compute-result 19690720)))