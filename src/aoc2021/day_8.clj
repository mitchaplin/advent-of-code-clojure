(ns aoc2021.day_8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def raw (slurp "resources/2021/day_8.txt"))
(def processed (map #(str/split % #" \| ") (str/split-lines raw)))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn get-freqs
  []
  (let [f (frequencies (flatten (map #(map count (str/split (second %) #" ")) processed)))]
    (+ (get f 2)
       (get f 3)
       (get f 4)
       (get f 7))))


;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn initialize-wire-map
  []
  {:0 nil
   :1 nil
   :2 nil
   :3 nil
   :4 nil
   :5 nil
   :6 nil
   :7 nil
   :8 nil
   :9 nil})

(defn break-line
  [line]
  (list (str/split (first line) #" ")
        (str/split (second line) #" ")))

(defn get-mappings
  [inp wire-map]
  (let [item (first inp)
        wm (assoc wire-map :1 (first (filter #(= (count %) 2) item)))
        wm (assoc wm :7 (first (filter #(= (count %) 3) item)))
        wm (assoc wm :4 (first (filter #(= (count %) 4) item)))
        wm (assoc wm :8 (first (filter #(= (count %) 7) item)))
        wm (assoc wm :9 (first (filter
                                 #(= (count (set/difference (set %)
                                                            (set/union (set (get wm :7))
                                                                       (set (get wm :4)))))
                                     1)
                                 (filter #(= (count %) 6) item))))
        wm (assoc wm :2 (first (filter #(= (count (set/difference (set (get wm :9)) (set %))) 2)
                                       (filter #(= (count %) 5) item))))
        threes-or-fives (set (map set (filter #(not (contains? (set (vals wm)) %)) (filter #(= (count %) 5) item))))
        threes (first (filter #(empty? (set/difference (set (get wm :1)) %)) threes-or-fives))
        fives (first (filter #(not (empty? (set/difference (set (get wm :1)) %))) threes-or-fives))
        wm (assoc wm :3 (first (filter #(= (set %) threes) (filter #(= (count %) 5) item))))
        wm (assoc wm :5 (first (filter #(= (set %) fives) (filter #(= (count %) 5) item))))
        zero-or-six (set (map set (filter #(not (contains? (set (map set (vals wm))) (set %))) (filter #(= (count %) 6) item))))
        zeros (first (filter #(empty? (set/difference (set (get wm :1)) %)) zero-or-six))
        sixes (first (filter #(not (empty? (set/difference (set (get wm :1)) %))) zero-or-six))
        wm (assoc wm :0 (first (filter #(= (set %) zeros) (filter #(= (count %) 6) item))))
        wm (assoc wm :6 (first (filter #(= (set %) sixes) (filter #(= (count %) 6) item))))]
    (list wm (second inp))))

(defn calc-total
  [wm item]
  (let [inv-map (clojure.set/map-invert (into {} (map #(vector (first %) (apply str (sort (second %)))) (map identity wm))))
        t (map #(Integer/parseInt (str (last (str (get inv-map (apply str (sort %))))))) item)]
    t))

(defn run
  []
  (loop
    [p processed
     stuff []]
    (if (empty? p)
      (reduce + (map #(Integer/parseInt %) (map #(apply str %) (partition-all 4 4 (apply str (apply concat stuff))))))
      (let [[x y] (get-mappings (break-line (first p)) (initialize-wire-map))
            new-nums (calc-total x y)]
        (recur (rest p)
               (cons new-nums stuff))))))






