(ns advent_2019_day_2
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2019/day_2.txt"))
(def processed (mapv #(Integer/parseInt %)
                     (str/split (str/trim-newline raw) #",")))

(defn determine-op
  [num]
  (if (= num 1)
    +
    *))

(defn grav-assist
  [input]
  (first (loop [iter 0
                arr input]
           (if (or (> iter (- (count arr) 1)) (= (get (subvec arr (+ iter 0) (+ iter 1)) 0) 99))
             arr
             (recur (+ iter 4)
                    (assoc arr (get arr (+ iter 3) 0)
                               ((determine-op (get arr iter))
                                (get arr (first (subvec arr (+ iter 1) (+ iter 2))))
                                (get arr (first (subvec arr (+ iter 2) (+ iter 3)))))))))))

(defn determine-input
  [input grav-assist]
  (loop [noun 0 verb 0 output 0 inp input]
    (if (= output 19690720)
      inp
      (do
        (if (>= verb 100)
          (recur (inc noun) 0 (grav-assist inp) (assoc inp 1 noun))
          (recur noun (inc verb) (grav-assist inp) (assoc inp 2 verb)))))))

