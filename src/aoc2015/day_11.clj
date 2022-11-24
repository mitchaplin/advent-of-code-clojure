(ns aoc2015.day-11
  (:require [clojure.string :as str]))

(def input "hxbxwxba")

(defn has-three-increasing?
  [i]
  (let [values (map #(map (fn [x] (apply int x)) %)
                    (partition 3 1 (str/split i #"")))
        t (some true? (map (fn [a b c] (< a b c) %) values))]
    t))

(defn contains-two-pairs?
  [i]
  (partition 2 1 (str/split i #""))
  (> (count (set (filter #(= (count %) 1) (map set (partition 2 1 (str/split i #"")))))) 1))

(defn check-rules
  [i]
  (cond (and (not (nil? (has-three-increasing? i)))
             (nil? (re-find #"i|o|l" i))
             (contains-two-pairs? i))
        i
        :else
        false))

(defn update-password
  [i]
  (loop [x (reverse i)
         final ""
         curr (first x)]
    (if (not= curr \z)
      (apply str (reverse (apply str final (char (inc (int curr))) (rest x))))
      (recur (rest x)
             (str final \a)
             (first (rest x))))))

(defn part-1
  []
  (loop [i input
         valid-password (check-rules i)]
    (if valid-password
      i
      (let [new-pass (update-password i)]
        (recur new-pass
               (check-rules new-pass))))))

(defn part-2
  []
  (loop [i input
         valid-password (check-rules i)
         found-count 0]
    (if (and valid-password (> found-count 1))
      i
      (let [new-pass (update-password i)
            pass-check (check-rules new-pass)]
        (recur new-pass
               pass-check
               (if pass-check (inc found-count) found-count))))))