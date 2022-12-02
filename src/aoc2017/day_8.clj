(ns aoc2017.day-8)

(ns aoc2017.day-8
  (:require [clojure.string :as str]
            [utils :as utils]))

(def raw (slurp "resources/2017/day_8.txt"))

(def processed
  (->> raw
       (str/split-lines)
       (map #(str/split % #" "))))

(defn resolve-fn
  [f]
  (if (= f "inc") + -))

(defn format-line
  "Pass each line here"
  [[to f amt _ from symb c]]
  (let [t (keyword to)
        fr (keyword from)]
    [t
     (resolve-fn f)
     (utils/parse-int amt)
     fr
     (cond (= symb "!=") not=
           (= symb "==") =
           :else (resolve (symbol symb)))
     (utils/parse-int c)]))

(defn format-input
  []
  (loop [p processed
         parsed-instructions []]
    (if (empty? p)
      parsed-instructions
      (let [formatted-line (format-line (first p))]
        (recur (rest p)
               (conj parsed-instructions formatted-line))))))

(defn update-keymap
  [km [to op x fr sym y]]
  (if (sym (or (get km fr) 0) y)
    (utils/update-with km to op x 0)
    km))

(defn part-1
  []
  (loop [c (format-input)
         updated-keymap {}]
    (if (empty? c)
      (val (apply max-key val updated-keymap))
      (recur (rest c)
             (update-keymap updated-keymap (first c))))))

(defn update-max-kv
  [km m]
  (let [new-max (val (apply max-key val km))]
    (if (> new-max m) new-max m)))

(defn part-2
  []
  (loop [c (format-input)
         updated-keymap {}
         max-val 0]
    (if (empty? c)
      max-val
      (let [updated-km (update-keymap updated-keymap (first c))]
        (recur (rest c)
               updated-km
               (update-max-kv updated-km max-val))))))