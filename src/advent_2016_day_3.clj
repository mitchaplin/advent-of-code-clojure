(ns advent_2016_day_3
  (:require [clojure.string :as str]))

(def formatted-input
  (let [raw (slurp "resources/2016/day_3.txt")]
    (->> raw
         (str/split-lines)
         (map #(str/trim %))
         (map #(str/split % #"  "))
         (map (fn [item]
                (map #(str/trim %) item)))
         (map (fn [item]
                (remove #(= "" %) item)))
         (map (fn [item]
                (map #(Integer/parseInt %) item))))))

(def fncts [first second last])

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn valid-triangles
  [inp]
  (count (remove false?
                 (map #(> (+ (first %)
                             (second %))
                          (last %))
                      inp))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn gather-triangles
  [fun]
  (map fun formatted-input))

(defn group-verticals
  []
  (let [verticals (map #(partition 3 %) (map #(gather-triangles %) fncts))]
    (loop
      [v verticals
       l []]
      (if (empty? v)
        l
        (recur (rest v) (concat (map #(sort %) (first v)) l))))))

(defn valid-triangles-2
  []
  (let [inp (group-verticals)]
    (count (remove false?
                   (map #(> (+ (first %)
                               (second %))
                            (last %))
                        inp)))))