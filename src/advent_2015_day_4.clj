(ns advent_2015_day_4
  (:require [clojure.string :as str]))

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(def secret-key "iwrupvqb")

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn find-lowest
  [secret-key]
  (loop
    [curr 1 current-key (str/join (concat secret-key (str curr))) current-hash (md5 current-key)]
    (if (= (str/join (first (partition 6 (str/split current-hash #"")))) "000000")
      curr
      (do
        (recur (inc curr) (str/join (concat secret-key (str (+ curr 1)))) (md5 current-key))))))


