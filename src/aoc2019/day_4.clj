(ns aoc2019.day_4
  (:require [clojure.string :as str]))

(def passwords (range 172851 675869))

(defn digits [n]
  (loop [result (list), n n]
    (if (pos? n)
      (recur (conj result (rem n 10))
             (quot n 10))
      result)))

(defn filter-passwords [pw]
  (filter (fn [pass]
            (apply <= (digits pass)))
          (filter #(< (count
                        (->> (digits %)
                             (partition-by identity)
                             (remove next)
                             (map first)))
                      6)
                  pw)))


(defn partition-passwords
  [pws]
  (map #(partition-by identity (digits %)) pws))

(defn filter-triples
  [password-list]
  (count (filter #(>=
                    (count (list (filter (fn [%] (<= 3 (count (digits %))))))) 0)
                 password-list)))

(defn get-counts
  [partitioned-passwords]
  (count
    (filter true?
            (map (fn [pw]
                   (some #(= 2 (count %)) pw))
                 partitioned-passwords))))