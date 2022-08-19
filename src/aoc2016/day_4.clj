(ns aoc2016.day_4
  (:require [clojure.string :as str]))


(def raw (slurp "resources/2016/day_4_practice.txt"))
(def processed (str/split-lines raw))

(defn format-data
  [d]
  (let [split (str/split d #"-")
        f (str/join (butlast split))
        l (str/split (last split) #"[\[\]]")]
    (conj [] f l)))

(defn format-data-2
  []
  (map format-data processed))

(defn remove-non-essentials
  [f i]
  (loop [x (map identity f)
         running (apply str (remove #(= % (first x)) i))]
    (if (empty? x)
      running
      (recur
        (rest x)
        (apply str (remove #(= % (first (rest x))) running))))))

(defn remove-non-matches
  [input]
  (loop [x (map identity (second (second input)))
         running (apply str (remove #(= % (first x)) (first input)))]
    (if (empty? x)
      (list (remove-non-essentials running (first input)) (second (second input)) (first (second input)))
      (recur
        (rest x)
        (apply str (remove #(= % (first (rest x))) running))))))

(defn totals
  [t]
  (if (true? (first t))
    (last t)
    0))

(defn determine-validity
  [inp]
  (let [freqs (frequencies (first inp))
        r (remove nil? (map #(get freqs %) (map identity (second inp))))
        s (if (empty? r) `(0) r)
        x (>= (count freqs) 5)
        _ (println freqs s x inp)
        k (apply >= s)
        j (every? true?
                  (map #(= (apply str (sort (clojure.string/split % #"")) %))
                       (map #(apply str %)
                            (map #(map first %)
                                 (map second (filter #(< 1 (count (second %)))
                                                     (group-by second freqs)))))))]
    _ (println freqs s x inp (and j k x))
    (list (and j k x) freqs (last (butlast inp)) (Integer/parseInt (last inp)))))

(defn run
  []
  (reduce + (map #(totals %)
                 (map #(determine-validity
                         (remove-non-matches %))
                      (format-data-2)))))