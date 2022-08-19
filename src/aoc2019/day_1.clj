(ns aoc2019.day_1
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2019/day_1.txt"))
(def processed
  (->> raw
       (str/split-lines)
       (map #(Integer/parseInt %))))

(defn calc_fuel
  [mass]
  (- (quot mass 3) 2))

(defn calc_one_mass
  [masses]
  (reduce + (map calc_fuel
                 masses)))

(defn total_mass
  [masses]
  (reduce + (map #(loop
                    [acc [%]]
                    (if (not (nat-int? (calc_fuel (last acc))))
                      (reduce + (rest (butlast acc)))
                      (recur (conj acc (calc_fuel (last acc))))))
                 masses)))



