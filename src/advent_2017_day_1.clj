(ns advent_2017_day_1
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2017/day_1.txt"))
(def processed (partition-by identity (map #(Integer/parseInt %) (str/split raw #""))))

(defn get-values
  [vals]
  (println vals)
  (if (< 2 (count vals))
    (reduce + (butlast vals))
    (first vals)))

(defn calc-total
  []
  (reduce + (map #(get-values %) (filter #(<= 2 (count %)) processed))))

(defn check-value
  [pointer new-pointer lst]
  (let [new-val (get lst pointer)]
    (if (= new-val (get lst new-pointer)) new-val 0)))

(defn calculate-next
  [curr-pointer lst]
  (let [v lst
        cnt (count v)
        x (+ curr-pointer (quot cnt 2))]
    (if (< cnt x)
      (* 2 (check-value curr-pointer (Math/abs (- cnt x)) lst))
      (* 2 (check-value curr-pointer x lst)))))

(defn pt-2
  []
  (let [l (vec (flatten processed))
        cnt (quot (count l) 2)]
    (loop [lst l
           pointer 0
           total 0]
      (if (< cnt pointer)
        total
        (recur (rest lst)
               (inc pointer)
               (+ total (calculate-next pointer l)))))))

