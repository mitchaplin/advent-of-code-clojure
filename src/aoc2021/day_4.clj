(ns aoc2021.day_4
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2021/day_4_practice.txt"))
(def processed (remove #(empty? %) (str/split raw #"\n")))
(def inst (map #(Integer/parseInt %) (str/split (first processed) #",")))
(def board (map (fn [x] (remove empty? x)) (map #(str/split % #" ") (rest processed))))

(defn line-to-numbers
  [line]
  (map #(Integer/parseInt %) (remove empty? (str/split line #" "))))

;(defn format-data
;  [input]
;  (let [[numbers & boards] (remove #(= (first %) "") (partition-by empty? input))]
;    {:numbers (map #(Integer/parseInt %) (str/split (first numbers) #","))
;     :boards (map #(map line-to-numbers %) boards)}))
;
;(defn mark-number
;  [n boards]
;  (map (nth boards n) boards))
;
