(ns aoc2017.day_3
  (:require [clojure.string :as str]))

(def input 368078)
(def test-input (range 1 26))
(def num-range (range input))

(defn build-board
  []
  (loop
    [x (count test-input)]))