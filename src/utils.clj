(ns utils
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo])
  (:import (java.io PushbackReader)))

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

(def ^:private inf Double/POSITIVE_INFINITY)

(defn update-costs
  "Returns costs updated with any shorter paths found to curr's unvisisted
  neighbors by using curr's shortest path"
  [g costs unvisited curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
      (fn [c nbr nbr-cost]
        (if (unvisited nbr)
          (update-in c [nbr] min (+ curr-cost nbr-cost))
          c))
      costs
      (get g curr))))

(defn dijkstra
  "Returns a map of nodes to minimum cost from src using Dijkstra algorithm.
  Graph is a map of nodes to map of neighboring nodes and associated cost.
  Optionally, specify destination node to return once cost is known"
  ([g src]
   (dijkstra g src nil))
  ([g src dst]
   (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
          curr src
          unvisited (disj (apply hash-set (keys g)) src)]
     (cond
       (= curr dst)
       (select-keys costs [dst])

       (or (empty? unvisited) (= inf (get costs curr))) costs :else (let [next-costs (update-costs g costs unvisited curr) next-node (apply min-key next-costs unvisited)] (recur next-costs next-node (disj unvisited next-node)))))))
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

(defn points-around [point]
  (remove (fn [p] (= point p))
          (apply combo/cartesian-product
                 (map #(range (dec %) (+ 2 %)) point))))

(defn points-around-inclusive [point]
  (sort-by second (apply combo/cartesian-product
                         (map #(range (dec %) (+ 2 %)) point))))