(ns advent_2020_day_7
  (:require [clojure.string :as str]))


(def raw (slurp "resources/2020/day_7.txt"))
(def processed (str/split-lines raw))

(defn arrange-bags
  [bags]
  (cond
    (= (first bags) "no other bags.") {}
    :else
    (reduce (fn [m content]
              (let [[_ n type] (re-find #"(\d+) (.*) bags?[.]?" content)]
                (assoc m type (Integer/parseInt n))))
            {} bags)))

(defn separate
  []
  (fn [initial group]
    (let [contents (str/split (last group) #", ")]
      (assoc initial
        (first group)
        (arrange-bags contents)))))

(defn parse-rules
  []
  (->> processed
       (map #(str/split % #" bags contain "))
       (reduce (separate) {})))


;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn find-starting-vals
  [s]
  (loop [r (parse-rules)
         acc []]
    (let [f (get (second (first r)) s)
          new-acc (if (not (nil? f))
                    (conj acc (ffirst r))
                    acc)]
      (if (empty? r)
        acc
        (recur (rest r) new-acc)))))

(defn gather-items
  []
  (loop
    [t (count (map first (parse-rules)))
     acc ["shiny gold"]]
    (if (zero? t)
      (dec (count acc))
      (recur (dec t)
             (set (flatten (concat acc
                                   (map #(find-starting-vals %) acc))))))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;(defn count-bags
;  []
;  (loop [r (parse-rules)
;         acc [1]
;         n {"shiny gold" 1, "shiny golf" 4}]
;    (let [f (get r (ffirst n))
;          new-acc (if (not (or (nil? f)
;                               (empty? f)))
;                    (list (conj acc
;                                (+ (second (first n))
;                                   (* (second (first n))
;                                      (reduce + (map second
;                                                     (get r (ffirst n)))))))
;                          (into {} (rest (cons n (get (parse-rules) (ffirst n))))))
;                    (list acc (into {} (rest n))))
;          _ (println "num" (second (first n)) new-acc)]
;      (if (empty? n)
;        (reduce + acc)
;        (recur r (first new-acc) (second new-acc))))))

(defn count-bags
  [parsed-input instruction]
  (if (empty? (parsed-input instruction))
    1
    (inc (apply + (map #(* (count-bags parsed-input %)
                           ((parsed-input instruction) %))
                       (keys (parsed-input instruction)))))))