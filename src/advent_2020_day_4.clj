(ns advent_2020_day_4
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2020/day_4.txt"))
(def processed (str/split raw #"\n\n"))
(def formatted (map #(str/split % #" ")
                    (map #(clojure.string/replace % #"\n" " ")
                         processed)))
(defn kv-pairs
  [s]
  (map #(str/split % #":") s))
(def data (map #(into (sorted-map) %) (map kv-pairs formatted)))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn calc-valid-passwords
  []
  (count (remove false?
                 (map #(or (= (count %) 8)
                           (= #{"hgt" "pid" "byr" "eyr" "iyr" "ecl" "hcl"} %))
                      (map #(set
                              (map (fn [[k v]] k) %))
                           data)))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn byr-validator
  [s]
  (not (nil? (utils/in? (range 1920 2003 1) (Integer/parseInt s)))))

(defn iyr-validator
  [s]
  (not (nil? (utils/in? (range 2010 2021 1) (Integer/parseInt s)))))

(defn eyr-validator
  [s]
  (not (nil? (utils/in? (range 2020 2031 1) (Integer/parseInt s)))))

(defn hgt-validator
  [s]
  (let [prefix (str/join (butlast (butlast s)))
        suffix (apply str (list (last (butlast s)) (last s)))
        cm-range (range 150 194 1)
        in-range (range 59 77 1)]
    (and (or (= suffix "in") (= suffix "cm"))
         (= (re-find #"\d+" prefix) prefix)
         (cond (= suffix "in")
               (not (nil? (utils/in? in-range (Integer/parseInt prefix))))
               (= suffix "cm")
               (not (nil? (utils/in? cm-range (Integer/parseInt prefix))))
               :else false))))

(defn hcl-validator
  [s]
  (let [prefix (first s)
        suffix (str/join (rest s))]
    (println suffix)
    (and (= prefix \#)
         (= (count suffix) 6)
         (= (re-find #"[0-9a-fA-F]+" suffix) suffix))))

(defn ecl-validator
  [s]
  (not (nil? (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} s))))

(defn pid-validator
  [s]
  (and (= (count s) 9)
       (= (re-find #"\d+" s) s)))

(defn calc-valid-passwords-2
  []
  (count (remove false?
                 (map #(and (or (= #{"hgt" "pid" "byr" "eyr" "iyr" "ecl" "hcl"} (set (keys %)))
                                (= #{"hgt" "pid" "byr" "eyr" "iyr" "ecl" "hcl" "cid"} (set (keys %))))
                            (byr-validator (get % "byr"))
                            (iyr-validator (get % "iyr"))
                            (eyr-validator (get % "eyr"))
                            (hgt-validator (get % "hgt"))
                            (hcl-validator (get % "hcl"))
                            (ecl-validator (get % "ecl"))
                            (pid-validator (get % "pid")))
                      data))))


