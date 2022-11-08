(ns aoc2018.day_5
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def raw (slurp "resources/2018/day_5.txt"))

(defn transform
  [curr nxt]
  (if (nil? nxt)
    1
    (cond (= curr nxt) 1
          (= (str/capitalize curr) (str nxt)) 2
          (= (str curr) (str/capitalize nxt)) 2

          :else
          1)))

(defn transform-polymer
  [r]
  (loop
    [p r
     newPolymer ""]
    (if (empty? p)
      newPolymer
      (let [op (transform (first p) (second p))]
        (recur (nthrest p op)
               (str newPolymer
                    (if (= op 1)
                      (first p)
                      "")))))))

(defn p1
  [r]
  (let [p (transform-polymer r)
        tp (transform-polymer p)]
    (if (= (count p) (count tp))
        (count tp)
        (p1 tp))))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(def letters ["aA" "bB" "cC" "dD" "eE" "fF" "gG" "hH" "iI" "jJ" "kK" "lL" "mM" "nN" "oO" "pP" "qQ" "rR" "sS" "tT" "uU" "vV" "wW" "xX" "yY" "zZ"])
(def removed-letters (map #(str/replace raw (re-pattern (str "["%"]")) "") letters))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn p2
  []
  (apply min (map p1 removed-letters)))

;(defn transform
;  [p]
;  (loop [[c & p] p
;         result []]
;    (let [r (first result)]
;      (cond
;        (nil? c) (apply str (reverse result))
;        (nil? r) (recur p (cons c result))
;        (and (not= c r)
;             (= (str/upper-case c)
;                (str/upper-case r))) (recur p (rest result))
;        :else (recur p (cons c result))))))
;
;(defn derive-combos
;  []
;  (map str (->> (range (int \a) (inc (int \z))) (map char))))

;(def combos (derive-combos))
;
;(defn apply-combos-to-polymers
;  [c]
;  (apply str (remove #(= (str/lower-case %) c) (into [] raw))))
;
;(defn idk
;  []
;  (set (map apply-combos-to-polymers combos)))
;
;(defn react-polymer
;  [p]
;  (->> p
;       (transform)
;       (count)))
;
;(defn react-polymer-p2
;  []
;  (apply min (map react-polymer (idk))))