(ns aoc2022.day-11
  (:require
    [clojure.math.numeric-tower :as math]
    [clojure.string :as str]))

(def raw (->> (utils/read-file "resources/2022/day_11.txt")
              (map str/trim)
              (partition-by empty?)
              (group-by count)
              (vals)
              (first)))

(def monkey-list-practice [:0 :1 :2 :3])
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
  (let [x (mapv utils/parse-int (str/split (str/replace (second (str/split items #": ")) #"," "") #" "))
        to-true (keyword (last (str/split to-true #" ")))
        to-false (keyword (last (str/split to-false #" ")))]
    {(keyword (str i))
     {:items        x
      :parse-fn     (parse-op (str/split (second (str/split val #"= ")) #" "))
      :divisible-by (utils/parse-int (last (str/split divi #" ")))
      :to-true      to-true
      :to-false     to-false
      :total-count  0}}))

(def initial-monkey-map (into {} (map-indexed (fn [i v] (parse-input i v)) raw)))

(def divi (reduce math/lcm (map :divisible-by (vals initial-monkey-map))))

(defn update-monkey-map
  [i cm mm is-p1]
  (let [new-val ((:parse-fn cm) i)
        updated-val (if is-p1 (Math/floorDiv new-val 3) (mod new-val divi))
        is-divisible? (= 0 (mod updated-val (:divisible-by cm)))]
    (if is-divisible? (assoc-in mm [(:to-true cm) :items] (conj (:items (get mm (:to-true cm))) updated-val))
                      (assoc-in mm [(:to-false cm) :items] (conj (:items (get mm (:to-false cm))) updated-val)))))

(defn monkey-bizz
  [m mm is-p1]
  (let [cm (m mm)
        updated-map (assoc-in mm [m :total-count] (+ (count (:items cm))
                                                     (:total-count cm)))]
    (loop [i (:items cm)
           monkey-map updated-map]
      (if (empty? i)
        (assoc-in monkey-map [m :items] [])
        (recur (rest i)
               (update-monkey-map (first i) cm monkey-map is-p1))))))

(defn part-1
  [m ml is-p1]
  (reduce (fn [new-map k]
            (monkey-bizz k new-map is-p1))
          m
          ml))

(defn run-times
  [n ml is-p1]
  (loop [iter n
         new-map initial-monkey-map]
    (if (zero? iter)
      new-map
      (recur (dec iter)
             (part-1 new-map ml is-p1)))))

(defn count-total
  [is-p1 num]
  (reduce * (take-last 2 (sort (map :total-count (vals (run-times num monkey-list is-p1)))))))

;(count-total false 10000)
;(count-total true 20)