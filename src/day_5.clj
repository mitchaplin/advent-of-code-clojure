(ns day_5
  (:require [clojure.string :as str]))

(def raw (slurp "resources/day_2.txt"))
(def processed (mapv #(Integer/parseInt %)
                     (str/split (str/trim-newline raw) #",")))

(defn determine-op [num] (if (= num 1) + *))

(defn grav-assist
  [input]
  (first (loop [iter 0 arr input]
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
        (println noun verb output)
        (if (>= verb 100)
          (recur (inc noun) 0 (grav-assist inp) (assoc inp 1 noun))
          (recur noun (inc verb) (grav-assist inp) (assoc inp 2 verb)))))))

(defn formatted-inst [inst] (cons (map #(Integer/parseInt %) (str/split (first (cons (format "%05d" (check-mode inst)) (rest inst))) #"")) (rest inst)))

(defn check-op
  [inst]
  (str/join (nthrest (first (formatted-inst inst)) (- (count (first (formatted-inst inst))) 2)))) ;"01"

(defn check-mode
  [inst]
  (Integer/parseInt (str/join (first inst))))


;mode 1 op 02 inst full
;1002,4,3,4,33
;; halt cond 3,0,4,0,99

;;3,50 put 3 at 50?
;;4,50 output the value at address 50

;(cons (reverse (list (last (take-nth (- (count '(1 0 0 2)) 1) '(1 0 0 2))) (last (take-nth (- (count '(1 0 0 2)) 2) '(1 0 0 2))))) '(4 3 4 3))
;(str/join (nthrest '(1 0 0 2) (- (count '(1 0 0 2)) 2)))

;Parameters that an instruction writes to will never be in immediate mode. ???

;what does 3,50 actually do?

;what are we outputting