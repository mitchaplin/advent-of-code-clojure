(ns aoc2022.day-12
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def raw (utils/read-file "resources/2022/day_12.txt"))

(def start [0 20])
(def end [88 20])
(def start-kw :020)
(def end-kw :8820)

;(def start [0 0])
;(def end [5 2])
;(def start-kw :00)
;(def end-kw :52)

(def max-length (dec (count (first raw))))
(def max-height (dec (count raw)))

(def idk (map #(apply str (map str %)) (partition (inc max-length) (str/replace (str/replace (apply str raw) #"E" "z") #"S" "a"))))

(def processed (into (sorted-map) (apply concat (map-indexed (fn [a x] (map-indexed (fn [b y] {(keyword (str b a))
                                                                                               {:val       (int y)
                                                                                                :neighbors {}}}) x)) idk))))

(def all-points (into [] (mapv vec (combo/cartesian-product (range (inc max-length)) (range (inc max-height))))))

(defn generate-neighbor-points
  [p]
  (let [v p
        ax (first v)
        ay (second v)
        points (remove (fn [[x y]] (or (and (= (inc ay) y) (= (inc ax) x))
                                       (and (= (inc ay) y) (= (dec ax) x))
                                       (and (= (dec ay) y) (= (inc ax) x))
                                       (and (= (dec ay) y) (= (dec ax) x))
                                       (> y max-height)
                                       (> x max-length)
                                       (neg? x)
                                       (neg? y)))
                       (utils/exclusive-points-around v))]
    points))

(def generated-neighbors
  (reduce (fn [km [a b]] (assoc-in km [(keyword (str a b)) :neighbors]
                                 (into (sorted-map) (remove nil? (mapv (fn [[x y]] (if (>= (inc (:val ((keyword (str a b)) processed)))
                                                                                           (:val ((keyword (str x y)) processed)))

                                                                                     {(keyword (str x y))
                                                                                      (:val ((keyword (str a b)) processed))} nil))
                                                                       (generate-neighbor-points [a b]))))))
          processed
          all-points))

(def graph (into (sorted-map) (map (fn [x] {(key x) (:neighbors (val x))}) generated-neighbors)))

(defn set-keys-to-1
  [collection]
  (reduce-kv (fn [acc k v] (assoc acc k 1)) {} collection))

(def initial-map (into (sorted-map) (reduce-kv (fn [acc k v] (assoc acc k (set-keys-to-1 v))) {} graph)))

;(utils/dijkstra testing start-kw end-kw)