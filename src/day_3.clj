(ns day_3
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [clojure.spec.alpha :as alpha]))
(use 'clojure.pprint)

(def test-data [["D3" "R5" "D3"] ["L3" "D5" "D3" "L5" "U3"]])


(def raw (slurp "resources/day_3.txt"))
(def input (map #(str/split % #",")(str/split raw #"\n")))

(defn gen-board [w h init-board]
  (mapv (fn [x]
          (mapv (fn [y] (init-board x y))
                (range w))) (range h)))

(defn isolate-steps [path] (if (not (= "" (str/join (rest path)))) (Integer/parseInt (str/join (rest path))) nil))
(defn isolate-marker [path] (if (not= (first path) "") (first path) nil))

(defn make-segment [[x y] dir length] (case dir
                                         \D [x (list y (+ length y))]
                                         \U [x (list y (- y length))]
                                         \L [(list x (- x length)) y]
                                         \R [(list x (+ length x)) y]))

(defn determine-direction
  [[x1 y1]]
  (if (list? x1)
    (if (> (first x1) (second x1)) \L \R)
    (if (> (first y1) (second y1)) \U \D)))

(defn determine-intersection
  [[x1 y1] [x2 y2]]
  (if (list? x1)
    (if (and (alpha/int-in-range? (first y2) (inc (second y2)) y1) (alpha/int-in-range? (first x1) (inc (second x1)) x2)) [x2 y1])
    (if (and (alpha/int-in-range? (first x2) (inc (second x2)) x1) (alpha/int-in-range? (first y1) (inc (second y1)) y2)) [x1 y2])))

(defn get-last-point [[x y]] (if (list? x) [(last x) y] [x (last y)]))

(defn point-in-seg [[x y] [x1 y1]]
  (if (list? x1)
    (and (alpha/int-in-range? (first x1) (inc (second x1)) x) (= y y1))
    (and (alpha/int-in-range? (first y1) (inc (second y1)) y) (= x x1))))

(defn length-of-segment [[x1 y1]]
  (if (list? x1)
    (count (range (first x1) (second x1)))
    (count (range (first y1) (second y1)))))

(defn length-to-point [[x y] [x1 y1]]
  (case (determine-direction [x1 y1])
    \D (- (Math/abs (second y1)) (Math/abs y))
    \U (- (Math/abs y) (Math/abs (first y1)))
    \L (- (Math/abs x) (Math/abs (first x1)))
    \R (- (Math/abs (second x1)) (Math/abs x))))

(defn get-line-length [input point] (loop
                                      [inp input count 0]
                                      (if (empty? inp)
                                        count
                                        (if (point-in-seg point (first inp))
                                          (+ count (length-to-point point (first inp)))
                                          (recur (rest inp) (+ count (length-of-segment (first inp))))))))

(defn make-segments [input] (loop [segments input
                                   acc []
                                   point [0 0]]
                              (if (empty? segments)
                                acc
                                (let [something (make-segment point
                                                              (isolate-marker (first segments))
                                                              (isolate-steps (first segments)))]
                                  (recur (rest segments)
                                         (conj acc something)
                                         (get-last-point something))))))

(defn filter-segments [segments func] (filter #(list? (func %)) segments))

(defn format-segments [segments] (list (filter-segments segments first) (filter-segments segments second)))

(defn compare-segments []
  (let [line1 (format-segments (make-segments (first input)))
        line2 (format-segments (make-segments (second input)))]
    (remove nil? (concat (reduce (fn [acc idk] (concat acc (map #(determine-intersection idk %) (second line2)))) '() (first line1))
                         (reduce (fn [acc idk] (concat acc (map #(determine-intersection idk %) (second line1)))) '() (first line2))))))

(defn map-intersection [segment coll]
  (remove nil? (reduce #(conj %1 (determine-intersection segment %2)) '() coll)))

(defn find-closest-intersection []
  (apply min (map (fn [[x y]] (+ (Math/abs x) (Math/abs y))) (compare-segments))))

(defn determine-width [] (reduce + (map #(isolate-steps %) input)))
(defn determine-height [] (reduce + (map #(isolate-steps %)) input))


(defn take-step [direction [x y]] (case direction
                                      \D [(inc x) y]
                                      \U [(dec x) y]
                                      \L [x (dec y)]
                                      \R [x (inc y)]))


(defn determine-marker [direction b pointer] (case direction
                                               \R (if (= (get-in b (take-step direction pointer)) ".") "-" (if (= (second b) 1) "-" "@"))
                                               \L (if (= (get-in b (take-step direction pointer)) ".") "-" (if (= (second b) 1) "-" "@"))
                                               \U (if (= (get-in b (take-step direction pointer)) ".") "|" (if (= (second b) 1) "|" "@"))
                                               \D (if (= (get-in b (take-step direction pointer)) ".") "|" (if (= (second b) 1) "|" "@"))))


;(def board (gen-board (determine-width) (determine-height) (constantly ".")))

(defn trace-wires [board input] (loop [data input
                                       pointer [10 10]
                                       stepper (isolate-steps (first data))
                                       b (first board)](if (empty? data)[b 1])
                                           (let
                                             [direction (isolate-marker (first data)) new-step (take-step direction pointer)]
                                             (if (zero? stepper)
                                               (recur
                                                 (rest data)
                                                 pointer
                                                 (isolate-steps (second data))
                                                 b)
                                               (recur
                                                 data
                                                 new-step
                                                 (dec stepper)
                                                 (assoc-in b [(first new-step) (last new-step)] (determine-marker direction b pointer)))))))

(defn find-intersections [data item]
  (let [n (count (first data))
        i (.indexOf (flatten data) item)]
    (if (pos? i)
      (list (quot i n) (mod i n)))))

(defn run-both-datasets [] (trace-wires (trace-wires board (first test-data)) (second test-data)))

(defn determine-closest-point [] "")



