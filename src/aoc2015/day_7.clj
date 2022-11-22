(ns aoc2015.day_7
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def raw (slurp "resources/2015/day_7.txt"))
(def processed (map #(str/split % #" -> ") (str/split-lines raw)))

(def initialized-map
  (->> processed
       (map #(keyword (last %)))))

(def instructions #{"AND" "OR" "NOT" "RSHIFT" "LSHIFT"})

(defn hydrate-values
  []
  (into {} (map vec (partition 2 (interleave initialized-map (repeat (count initialized-map) 0))))))

(defn instruction-contains-operator?
  [inst]
  (not (empty? (set/intersection instructions (set (str/split inst #" "))))))

(defn determine-op-type
  [value]
  (cond (str/includes? value "AND") bit-and
        (str/includes? value "OR") bit-or
        (str/includes? value "NOT") bit-not
        (str/includes? value "LSHIFT") bit-shift-left
        (str/includes? value "RSHIFT") bit-shift-right))

(defn assign-value
  [value wire-map to]
  (let [map-val (get wire-map (keyword value))]
    (if (nil? map-val)
      (assoc wire-map (keyword to) (utils/parse-int (str value)))
      (assoc wire-map (keyword to) map-val))))

(defn derive-number
  [[x _ z] f wire-map to]
  (let [first-map-val (get wire-map (keyword x))
        second-map-val (get wire-map (keyword z))
        final-map-x (if (nil? first-map-val)
                      (utils/parse-int x)
                      first-map-val)
        final-map-z (if (nil? second-map-val)
                      (utils/parse-int z)
                      second-map-val)]
    (assoc wire-map (keyword to) (f final-map-x final-map-z))))


(defn calculate-value
  [value wire-map to]
  (let [op-fn (determine-op-type value)
        split-val (str/split value #" ")]
    (cond (= (count split-val) 3)
          (derive-number split-val op-fn wire-map to)
          (= (count split-val) 2)
          (assign-value (- 65535 (get wire-map (keyword (second split-val))))
                        wire-map
                        to))))

(defn string-is-number?
  [s]
  (every? #(Character/isDigit %) s))

(defn has-necessary-inputs
  [wire-map value]
  (let [x (str/split value #" ")]
    (cond (and (= (count x) 1)
               (get wire-map (keyword (first x))))
          true

          (and (= (count x) 1)
               (string-is-number? (first x)))
          true

          (= (count x) 2)
          (some? (get wire-map (keyword (second x))))

          (= (count x) 3)
          (or (and (string-is-number? (first x))
                   (some? (get wire-map (keyword (last x)))))
              (and (some? (get wire-map (keyword (first x))))
                   (string-is-number? (last x)))
              (and (string-is-number? (first x))
                   (string-is-number? (last x)))
              (and (some? (get wire-map (keyword (first x))))
                   (some? (get wire-map (keyword (last x)))))))))

(defn update-wire-map
  [wire-map value to]
  (if (has-necessary-inputs wire-map value)
    (let [updated-value (if (instruction-contains-operator? value)
                          (calculate-value value wire-map to)
                          (assign-value value wire-map to))]
      updated-value)
    wire-map))

(defn assign-wires
  [wm]
  (loop [p processed
         wire-map wm]
    (if (empty? p)
      wire-map
      (let [next-iter (rest p)
            [value to] (first p)]
        (recur next-iter
               (update-wire-map wire-map value to))))))

(defn part-1
  []
  (loop [wire-map {}]
    (if (= (count wire-map)
           (count processed))
      (get wire-map :a)
      (recur (assign-wires wire-map)))))