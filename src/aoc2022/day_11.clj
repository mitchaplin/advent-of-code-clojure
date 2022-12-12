(ns aoc2022.day-11
  (:require [clojure.string :as str]))

(def raw (last (first (group-by count (partition-by empty? (map str/trim (utils/read-file "resources/2022/day_11.txt")))))))
(def monkey-list [:0 :1 :2 :3 :4 :5 :6 :7])

(defn parse-op
  [[a sym b]]
  (cond
    (and (= b "old") (= a "old"))
    (fn [old] ((resolve (symbol sym)) old old))

    (and (not= b "old") (= a "old"))
    (fn [old] ((resolve (symbol sym)) old (utils/parse-int (str b))))

    (and (not= a "old") (= b "old"))
    (fn [old] ((resolve (symbol sym)) (utils/parse-int (str a)) old))

    :else
    (fn [] ((resolve (symbol sym))
            (utils/parse-int (str a))
            (utils/parse-int (str b))))))

(defn parse-input
  [i [_ items val divi to-true to-false]]
  (let [x (mapv utils/parse-int (str/split (str/replace (second (str/split items #": "))  #"," "") #" "))
        to-true (keyword (last (str/split to-true #" ")))
        to-false (keyword (last (str/split to-false #" ")))]
    {(keyword (str i))
     {:items x
      :parse-fn (parse-op (str/split (second (str/split val #"= ")) #" "))
      :divisible-by (utils/parse-int (last (str/split divi #" ")))
      :to-true to-true
      :to-false to-false
      :total-count (count x)}}))

(defn update-monkey-map
  [i cm mm]
  (println i)
  (let [current-map cm
        new-val ((:parse-fn current-map) i)
        is-divisible? (= 0 (mod new-val (:divisible-by current-map)))
        updated-val (int (Math/floor (/ new-val 3)))]
    (if is-divisible? (assoc-in mm [(:to-true current-map) :items] (vec (cons updated-val (:items current-map))))
                      (assoc-in mm [(:to-false current-map) :items] (vec (cons updated-val (:items current-map)))))))

(defn monkey-bizz
  [m mm]
  (println m (:to-true (m mm)) mm)
  (let [cm (m mm)
        updated-map (assoc-in mm [m :total-count] (+ (count (:items cm))
                                                     (:total-count cm)))]
    (loop [i (:items cm)
           monkey-map updated-map]
      (if (empty? i)
        (assoc monkey-map [m :items] [])
        (recur  (rest i)
                (update-monkey-map (first i) cm monkey-map))))))

(def initial-monkey-map (into {} (map-indexed  (fn [i v] (parse-input i v)) raw)))

(defn part-1
  []
  (reduce (fn [new-map k]
              (monkey-bizz k new-map))
          initial-monkey-map
          monkey-list))
