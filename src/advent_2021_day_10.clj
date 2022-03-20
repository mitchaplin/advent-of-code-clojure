(ns advent_2021_day_10
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2021/day_10.txt"))
(def processed (str/split-lines raw))

;; if right put into Q
;; if left is found in Q, pop
;; if data is empty and Q is empty = perfect
;; if leftover = corrupt
;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn translate-to-mirror
  [c]
  (cond (= c \()
        \)
        (= c \{)
        \}
        (= c \[)
        \]
        (= c \<)
        \>))

(def rights #{\{ \( \< \[})
(def lefts #{\} \) \> \]})

(defn translate-char-to-value
  [c]
  (cond (= c \))
        3
        (= c \])
        57
        (= c \})
        1197
        (= c \>)
        25137))

(defn update-stack
  [c stack]
  (cond (contains? lefts c)
        (if (= (translate-to-mirror (first stack)) c)
         (rest stack)
         (cons c stack))

        (contains? rights c)
        (cons c stack)))

(defn calculate-total
  [stack]
  (if (some true? (map #(contains? lefts %) stack))
    (translate-char-to-value (first stack))
    0))

(defn garbo
  [item]
  (loop [current-stack []
         c item]
    (if (or (empty? c) (some true? (map #(contains? lefts %) current-stack)))
      (calculate-total current-stack)
      (recur (update-stack (first c) current-stack)
             (rest c)))))

(defn traverse-and-tally-inputs
  []
  (reduce + (map garbo processed)))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn translate-to-mirror-2
  [c]
  (cond (= c \()
        \)
        (= c \{)
        \}
        (= c \[)
        \]
        (= c \<)
        \>))

(defn translate-char-to-value-2
  [c current-val]
  (cond (= c \))
        (inc (* 5 current-val))
        (= c \])
        (+ 2 (* 5 current-val))
        (= c \})
        (+ 3 (* 5 current-val))
        (= c \>)
        (+ 4 (* 5 current-val))))

(defn garbo-2
  [item]
  (loop [current-stack []
         c item]
    ()
    (if (or (empty? c) (some true? (map #(contains? lefts %) current-stack)))
      current-stack
      (recur (update-stack (first c) current-stack)
             (rest c)))))

(defn idk-man
  [s]
  (loop [acc 0
         rems s]
    (if (empty? rems)
      acc
      (let [f (first rems)
            next-acc (translate-char-to-value-2 f acc)]
        (recur next-acc
               (rest rems))))))

(defn calc-vals
  [leftover-stack-vals]
  (if (some true? (map #(contains? lefts %) leftover-stack-vals))
    0
    (idk-man (map translate-to-mirror-2 leftover-stack-vals))))

(defn traverse-and-tally-inputs-2
  []
  (utils/median (remove #(zero? %) (map calc-vals (map garbo-2 processed)))))