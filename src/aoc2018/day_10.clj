(ns aoc2018.day-10
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [utils :as utils]))

(def raw (slurp "resources/2018/day_10.txt"))

;PART 1 & 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(def processed
  (->> (str/split-lines raw)
       (map #(re-seq #"-?\d+" %))
       (map (fn [x] (map #(Integer/parseInt %) x)))))

(defn calculate-movements
  [[x y xm ym]]
  (list (+ x xm) (+ y ym) xm ym))

(defn move-points
  [p]
  (map calculate-movements p))

(defn check-end-condition
  [points]
  (let [yArr (map second points)
        dist (count (range (apply min yArr) (apply max yArr)))]
    (= 8 (dec dist))))

(defn do-end-condition-check
  [points]
  (true? (check-end-condition points)))

(defn calculate-points
  [initial]
  (loop [p (move-points initial)
         iter 1]
    (if (do-end-condition-check p)
      (list iter (map #(list (first %) (second %)) p))
      (recur (move-points p)
             (inc iter)))))

(defn draw-board
  []
  (let [x (calculate-points processed)
        cp (second x)
        total-iters (first x)
        _ (println "TOTAL: " total-iters)
        xmin (apply min (map first cp))
        xmax (apply max (map first cp))
        ymin (apply min (map second cp))
        ymax (apply max (map second cp))]
    (map #(apply str %)
         (map #(map (fn [x] (if (utils/in? cp x) "X" ".")) %)
              (vals (into (sorted-map)
                          (group-by second
                                    (combo/cartesian-product
                                      (range xmin (inc xmax))
                                      (range ymin (inc ymax))))))))))