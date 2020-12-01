(ns advent_2015_day_6
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2015/day_6.txt"))
(def processed (map #(str/split % #" ")(str/split-lines raw)))

(def test-instructions '((["turn" "on"] "2,6" "through" "4,9") ["toggle" "7,4" "through" "9,8"] ["toggle" "5,9" "through" "5,9"] (["turn" "on"] "5,2" "through" "9,9") (["turn" "off"] "1,2" "through" "6,5")))

(defn parse-args
  []
  (map #(if (= (first %) "turn")
          (cons (conj [] (first %) (second %))
                (nthrest % 2)) %) processed))

(defn map-points []
  (map #()))