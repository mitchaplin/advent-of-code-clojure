(ns aoc2015.day_6
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def raw (slurp "resources/2015/day_6.txt"))
(def processed (map #(str/split % #" ") (str/split-lines raw)))

(defn parse-input
  []
  (map
    #(if (= (first %) "turn")
       (vec (rest %))
       %)
    processed))

(defn toggle-lights
  [b d c]
  (let [x (str/split b #",")
        y (str/split d #",")
        cp1 (range (Integer/parseInt (first x)) (inc (Integer/parseInt (first y))))
        cp2 (range (Integer/parseInt (second x)) (inc (Integer/parseInt (second y))))
        cp (set (combo/cartesian-product cp1 cp2))]
    (clojure.set/union (clojure.set/difference cp c) (clojure.set/difference c cp))))

(defn add-lights
  [b d c]
  (let [x (str/split b #",")
        y (str/split d #",")
        cp1 (range (Integer/parseInt (first x)) (inc (Integer/parseInt (first y))))
        cp2 (range (Integer/parseInt (second x)) (inc (Integer/parseInt (second y))))
        cp (set (combo/cartesian-product cp1 cp2))]
    (clojure.set/union c cp)))

(defn remove-lights
  [b d c]
  (let [x (str/split b #",")
        y (str/split d #",")
        cp1 (range (Integer/parseInt (first x)) (inc (Integer/parseInt (first y))))
        cp2 (range (Integer/parseInt (second x)) (inc (Integer/parseInt (second y))))
        cp (set (combo/cartesian-product cp1 cp2))]
    (clojure.set/difference c cp)))

(defn flip-lights
  [[a b _ d] curr]
  (cond
    (= a "on")
    (add-lights b d curr)
    (= a "off")
    (remove-lights b d curr)
    (= a "toggle")
    (toggle-lights b d curr)))


(defn compute
  []
  (loop
    [current #{}
     inp (parse-input)]
    (if (empty? inp)
      (count current)
      (recur (flip-lights (first inp) current)
             (rest inp)))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn update-curr
  [c new-item new-tot]
  (if false
    (list (let [[x y] (split-with (partial not= new-item) c)]
            (concat x (rest y))) (dec new-tot))
    (list c new-tot)))

(defn check-removal-validity
  [cp c]
  (loop
    [[curr new-total] [c 0]
     new-items cp]
    (if (empty? new-items)
      (list curr new-total)
      (recur (update-curr curr (first new-items) new-total)
             (rest new-items)))))

(defn toggle-lights-2
  [b d c]
  (let [x (str/split b #",")
        y (str/split d #",")
        cp1 (range (Integer/parseInt (first x)) (inc (Integer/parseInt (first y))))
        cp2 (range (Integer/parseInt (second x)) (inc (Integer/parseInt (second y))))
        cp (combo/cartesian-product cp1 cp2)
        total (* 2 (count cp))]
    (list (concat c cp) total)))

(defn add-lights-2
  [b d c]
  (let [x (str/split b #",")
        y (str/split d #",")
        cp1 (range (Integer/parseInt (first x)) (inc (Integer/parseInt (first y))))
        cp2 (range (Integer/parseInt (second x)) (inc (Integer/parseInt (second y))))
        cp (combo/cartesian-product cp1 cp2)
        total (count cp)]
    (list (concat c cp) total)))

(defn remove-lights-2
  [b d c]
  (let [x (str/split b #",")
        y (str/split d #",")
        cp1 (range (Integer/parseInt (first x)) (inc (Integer/parseInt (first y))))
        cp2 (range (Integer/parseInt (second x)) (inc (Integer/parseInt (second y))))
        cp (combo/cartesian-product cp1 cp2)]
    (check-removal-validity cp c)))

(defn flip-lights-2
  [[a b _ d] curr]
  (cond
    (= a "on")
    (add-lights-2 b d curr)
    (= a "off")
    (remove-lights-2 b d curr)
    (= a "toggle")
    (toggle-lights-2 b d curr)))

(defn compute-2
  []
  (loop
    [current []
     inp (parse-input)
     total 0
     [inst tot] (flip-lights-2 (first inp) current)
     _ (println (count inp))]
    (if (empty? inp)
      total
      (recur inst
             (rest inp)
             (+ tot total)
             (flip-lights-2 (first inp) current)
             (println (count inp))))))
