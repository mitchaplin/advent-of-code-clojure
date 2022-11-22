(ns aoc2019.day_5
  (:require
    [clojure.string :as str]
    [clojure.core.async :as async :refer [thread chan >! <! >!! <!! go]]))

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
        offset (ops (+ pc 3))
        sum (+ (ops op1) (ops op2))]
    (assoc ops offset sum)))

(defn execute-multiply
  [ops pc]
  (let [op1 (ops (inc pc))
        op2 (ops (+ pc 2))
        offset (ops (+ pc 3))
        prd (* (ops op1) (ops op2))]
    (assoc ops offset prd)))

;; TODO: overwrite
(defn execute-overwrite
  [ops pc idx]
  (let [op1 (ops (inc pc))
        op2 (ops (+ pc 2))
        offset (ops (+ pc 3))
        prd (* (ops op1) (ops op2))]
    (assoc ops offset prd)))

(defn execute-lookup
  [ops pc out]
  (thread (>!! out (ops pc)))
  ops)

;; TODO: make sure pc values are correct
(defn execute-ops
  [noun verb opcodes in out]
  (let [opcodes (assoc opcodes 1 noun 2 verb)]
    (loop [opcodes opcodes
           pc 0]
      (println opcodes)
      (cond
        (= (opcodes pc) 99)
        (opcodes 0)

        (= (opcodes pc) 1)
        (recur (execute-add opcodes pc)
               (+ pc 4))

        (= (opcodes pc) 2)
        (recur (execute-multiply opcodes pc)
               (+ pc 4))

        (= (opcodes pc) 3)
        (recur (execute-overwrite opcodes pc in)
               (+ pc 2))

        (= (opcodes pc) 4)
        (recur (execute-lookup opcodes pc out)
               (+ pc 2))))))

(defn compute-result
  [opcodes]
  (let [input (chan)
        output (chan)]
    (thread (>!! input 1))
    (loop [noun 0
           verb 0]
      (println "test2")
      (let [result (execute-ops noun verb opcodes input output)
            out (thread (<!! output))]
        (cond
          (not (nil? out)) out
          (< verb 99)  (recur noun (inc verb))
          (< noun 99)  (recur (inc noun) 0))))))

(defn part-1
  []
  (->> processed
       (compute-result)))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~