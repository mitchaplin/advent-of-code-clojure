(ns aoc2016.day_5
  (:require [clojure.string :as str]))

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(def raw (slurp "resources/2016/day_5.txt"))
(def processed (first (str/split-lines raw)))

(defn md5
  [^String s]
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

(defn replace-password
  [pass [r l]]
  (let
    [idx (read-string (str r))]
    (if (and (= (get pass idx) \#) (number? idx))
      (update pass idx #(first (str l %)))
      pass)))

(defn check-pass
  [inp password]
  (let
    [i (md5 inp)]
    (if (every? #(= % \0) (take 5 i))
      (replace-password password (nthrest (take 7 i) 5))
      password)))

(defn compute-password
  [inp]
  (loop
    [password [\# \# \# \# \# \# \# \#]
     idx 0]
    (if (not (some #(= \# %) password))
      (apply str password)
      (recur (check-pass (str inp idx) password)
             (inc idx)))))