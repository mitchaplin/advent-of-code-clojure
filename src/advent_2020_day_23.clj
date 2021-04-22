(ns advent_2020_day_23
  (:require [clojure.string :as str]))

"716892543"

{:7 :1
 :1 :6
 :6 :8
 :8 :9
 :9 :2
 :2 :5
 :5 :4
 :4 :3
 :3 :7}

;; generate this on the fly?

(defn generate-map-linked-list
  [padding]
  ;(range 10 51)
  (concat [7 1 6 8 9 2 5 4 3]))