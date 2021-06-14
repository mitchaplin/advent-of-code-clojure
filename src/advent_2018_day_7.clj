(ns advent_2018_day_7
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2018/day_7.txt"))
(def processed (str/split-lines raw))

(defn get-letters
  [line]
  (list (str (nth line 5)) (str  (nth line 36))))

(defn accumulate-letters
  []
 (map get-letters processed))

(defn example []
  (keys (reduce (fn [g edge]
                  (let [[from to] (map keyword (str/split edge #" "))]
                    (update g from #(conj % to))))
                {}
                (vec (map #(str (first %) " " (second %))
                          (accumulate-letters))))))