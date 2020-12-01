(ns advent_2019_day_10
  (:require [clojure.string :as str]))

(def raw (slurp "resources/day_10_test.txt"))
(def input (str/split (str/trim-newline raw) #"[)]"))

;; build vec of rows
;; loop through each asteroid in the entire map???
;; loop through each row, check index of initial asteroid
;; for each additional row looping, +1 and -1 and see if asteroid is there
;; ---- while each row


;; after all previously marked spots are now marked as

;; los... row + dist over? are how far away the children need to be placed