(ns aoc2021.day_13
  (:require
    [clojure.string :as str]))

(def raw (slurp "resources/2021/day_13.txt"))

(defn parse-instruction
  [line]
  (let [[_ axis amount] (re-matches #"fold along ([xy])=(\d+)" line)]
    [(keyword axis) (Integer/parseInt amount)]))

(defn parse-next-insts
  [input]
  (let [[dots folds] (-> (str/replace input "\r" "")
                         (str/split #"\n\n"))]
    [(->> (str/split-lines dots)
          (map #(mapv (fn [x] (Integer/parseInt x)) (str/split % #",")))
          set)
     (map parse-instruction (str/split-lines folds))]))

(defn fold
  [dots [dir fold-line]]
  (let [idx ({:x 0 :y 1} dir)]
    (->> dots
         (map (fn [dot]
                (let [v (dot idx)]
                  (if (<= v fold-line)
                    dot
                    (update dot idx (partial - (* 2 fold-line)))))))
         set)))

(defn part1
  [input]
  (let [[dots [instruction]] (parse-next-insts input)]
    (count (fold dots instruction))))

(defn print-dots [dots]
  (let [mx (apply min (map first dots))
        my (apply min (map second dots))
        x (apply max (map first dots))
        y (apply max (map second dots))]
    (run! println (map (fn [y]
                         (apply str (map #(if (dots [% y]) (str "O") \space)
                                         (range mx (inc x)))))
                       (range my (inc y))))))

(defn part2
  [input]
  (let [[dots instructions] (parse-next-insts input)]
    (print-dots (reduce fold dots instructions))))