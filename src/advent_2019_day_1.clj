(ns advent_2019_day_1
  (:require [clojure.string :as str]))

(def raw (slurp "resources/day_1.txt"))
(def processed (mapv #(Integer/parseInt %)
                     (str/split (str/trim-newline raw) #",")))

(defn calc_one_mass
  [masses]
  (reduce +
          (map #(int (- (Math/floor (/ % 3)) 2))
               masses)))

(defn calc_mass
  [mass]
  (int (- (Math/floor (/ mass 3)) 2)))

(defn total_mass
  [masses]
  (reduce + (flatten (map #(loop
                             [acc [%]]
                             (if (<= (calc_mass (last acc)) 0)
                               (rest acc)
                               (recur (conj acc (if (< (calc_mass (last acc)) 0) 0 (calc_mass (last acc))))))) masses))))


