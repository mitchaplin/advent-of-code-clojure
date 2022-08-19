(ns aoc2020.day_8
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2020/day_8.txt"))
(def processed (str/split-lines raw))

(defn check-dupe-inst?
  [p n]
  (utils/in? p n))

(defn check-last-inst
  [p d]
  (= (count d) p))

(defn get-next-inst
  [inst pointer acc]
  (cond (= (subs inst 0 3) "nop")
        (list (inc pointer) acc)
        (= (subs inst 0 3) "jmp")
        (list (+ pointer (Integer/parseInt (subs inst 4))) acc)
        (= (subs inst 0 3) "acc")
        (list (inc pointer) (+ (Integer/parseInt (subs inst 4)) acc))))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn execute-inst
  [data]
  (loop [inst (get data 0)
         pointer 0
         acc 0
         visited [0]]
    (let [n (get-next-inst inst pointer acc)
          ;_ (println n)
          dupe (check-dupe-inst? visited (first n))]
      (if dupe
        (list dupe acc)
        (recur (get data (first n))
               (first n)
               (second n)
               (cons pointer visited))))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn check-inst-is-jmp
  [inst idx]
  (if (= (subs inst 0 3) "jmp")
    idx
    nil))

(defn get-all-jmps
  [data]
  (loop [d data
         acc []
         idx 0]
    (if (empty? d)
      (remove nil? acc)
      (recur (rest d)
             (cons (check-inst-is-jmp (first d) idx) acc)
             (inc idx)))))

(defn mutate-data
  [jmps data]
  (loop
    [j jmps
     d [data]]
    (if (empty? j)
      d
      (recur (rest j)
             (conj d (update data (first j)
                             #(str "nop " (subs % 4))))))))


(defn execute-inst-2
  [data]
  (loop [inst (get data 0)
         pointer 0
         acc 0
         visited [0]]
    (let [n (get-next-inst inst pointer acc)
          dupe (check-dupe-inst? visited (first n))]
      (if (or dupe (check-last-inst (first n) data))
        (list (check-last-inst (first n) data) acc)
        (recur (get data (first n))
               (first n)
               (second n)
               (cons pointer visited))))))

(defn cycle-jmps
  []
  (first (remove nil?
                 (map #(if (= (first %) false)
                         nil
                         (second %))
                      (map execute-inst-2 (mutate-data (get-all-jmps processed)
                                                       processed))))))