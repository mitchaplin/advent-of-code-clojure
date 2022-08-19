(ns aoc2019.day_10
  (:require [clojure.string :as str]))

(def raw (slurp "resources/day_10_test.txt"))
(def input (str/split (str/trim-newline raw) #"[)]"))