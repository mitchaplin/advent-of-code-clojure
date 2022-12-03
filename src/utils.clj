(ns utils
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo])
  (:import (java.io PushbackReader)))

(defn read-file [file]
  (with-open [reader (clojure.java.io/reader file)]
    (reduce conj [] (line-seq reader))))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (if (nil? (some #(= elm %) coll))
    false
    true))

(defn load-edn
  [resource-file]
  (edn/read (PushbackReader. (io/reader (io/resource resource-file)))))

; fnil inc into map, add it if key does not exist without null pointer exception
; (update {} :a inc)
; (update {} :a (fnil inc 0))

(defn parse-int [s] (Integer/parseInt s))
(defn parse-binary [s] (Integer/parseInt s 2))

(defn split-blank-line
  [input]
  (-> (str/replace input "\r" "")
      (str/split #"\n\n")))

(defn abs [^long n] (Math/abs n))

(defn summation [n]
  (-> (* n (inc n))
      (/ 2)))

(defn char->int [c] (- (int c) 48))

(defn string-is-number?
  [s]
  (every? #(Character/isDigit %) s))

(defn update-values
  [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(defn lower-case? [s] (every? #(Character/isLowerCase ^char %) s))
(defn upper-case? [s] (every? #(Character/isUpperCase ^char %) s))

(defn seq-contains?
  "Determine whether a sequence contains a given item"
  [sequence item]
  (if (empty? sequence)
    false
    (reduce #(or %1 %2) (map #(= %1 item) sequence))))

(defn explode [coll n]
  (reduce (fn [colls xs]
            (map #(if %2 (conj %1 %2) %1)
                 colls
                 (concat xs (repeat nil))))
          (repeat n [])
          (partition-all n coll)))

(defn number-sort
  [mylist]
  (apply <= mylist))

(defn multiple?
  [x & factors]
  (some zero? (map #(rem x %) factors)))


(defn cost-from
  "What is the edge cost of going from one vertex to another connected vertex"
  [weighted-graph start goal]
  (get-in weighted-graph [start goal]))

(defn alternating-pairs
  "Turn a sequence into alternating pairs
   i.e [:a :b :c] -> [[:a :b] [:b :c]]"
  [coll]
  (let [[x y] coll
        xs (rest coll)]
    (if (empty? xs)
      []
      (->> (conj [] [x y] (alternating-pairs xs))
           flatten
           (partition 2)))))

(defn dfs
  "Depth first search to find all paths to a goal"
  [graph goal]
  (fn search
    [path visited]
    (let [current (peek path)]
      (if (= goal current)
        [path]
        (->> current graph keys
             (remove visited)
             (mapcat #(search (conj path %) (conj visited %))))))))

(defn find-paths
  "Find all paths in a directed graph"
  [graph start goal]
  (let [visited-nodes #{start}]
    ((dfs graph goal) [start] visited-nodes)))

(defn path-cost
  "Given a path i.e [:a :b :c] and a weighted graph g
   what is the total cost of this path?"
  [g path]
  (let [pairs (alternating-pairs path)]
    (->> (map #(apply (partial cost-from g) %) pairs)
         (reduce +))))

(defn cost
  "Find the costs of all paths in a weighted directed graph"
  [weighted-graph start goal]
  (let [all-paths (find-paths weighted-graph start goal)]
    (map (partial path-cost weighted-graph) all-paths)))

(defn min-cost [weighted-graph start goal]
  (apply min
         (cost weighted-graph start goal)))

(defn diff
  [vals]
  (map - (next vals) vals))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway) ; (1)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))

(defn encode-hex
  [s]
  (apply str (map #(format "%02x" %) (.getBytes s "UTF-8"))))

(defn decode-hex
  [s]
  (let [bytes (into-array Byte/TYPE
                          (map (fn [[x y]]
                                 (unchecked-byte (Integer/parseInt (str x y) 16)))
                               (partition 2 s)))]
    (String. bytes "UTF-8")))

(defn points-around
  ([point]
   (points-around point identity))
  ([point filter-fn]
   (->> point
        (map #(range (dec %) (+ 2 %)))
        (apply combo/cartesian-product)
        (filter filter-fn))))

(defn exclusive-points-around
  [point]
  (points-around point (fn [x] (not= x point))))

(defn inclusive-range
  [a b]
  (range a (inc b)))

(defn update-with
  [km k f v default-value]
  (merge km {k (f (or (get km k) default-value) v)}))