(ns advent_2020_day_13
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2020/day_13.txt"))
(def processed (str/split-lines raw))

(def departure (Integer/parseInt (first processed)))
(def cleaned-times (map #(Integer/parseInt %)
                        (remove #(= % "x")
                                (str/split (second processed) #","))))

(defn get-totals
  []
  (map #(list (- (apply max %) departure)
              (* (- (apply max %) departure) (first %))
            (first %))
     (map #(reductions + (take (inc (Math/toIntExact (/ departure %)))
                               (repeatedly (fn [] (+ % 0))))) cleaned-times)))
