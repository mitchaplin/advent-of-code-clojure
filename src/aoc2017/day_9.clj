(ns aoc2017.day-9
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2017/day_9.txt"))

(defn replace-all-escapes
  [start]
  (loop [s start
         transformed (str/replace-first s #"!." "")]
    (if (= (count s) (count transformed))
      transformed
      (recur transformed
             (str/replace-first transformed #"!." "")))))

(defn update-group-membership
  ([c escaped groups p2?]
   (cond (and (= escaped true) (not= c ">"))
         (list true groups (if p2? 1 0))

         (and (= escaped true) (= c ">"))
         (list false groups 0)

         (and (= escaped false) (= c "<"))
         (list true groups 0)

         (and (= escaped false) (= c "{"))
         (list false (conj groups c) 0)

         (and (= escaped false) (= c "}"))
         (list false (butlast groups) (if p2? 0 (count groups)))

         :else
         (list escaped groups 0))))

;PART 1 & 2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn count-groups
  [s p2?]
  (loop [c s
         groups []
         is-escaped false
         total 0]
    (if (empty? c)
      total
      (let [[escaped groups group-score] (update-group-membership (str (first c)) is-escaped groups p2?)]
        (recur (rest c)
               groups
               escaped
               (+ total (or group-score 0)))))))

;(count-groups (replace-all-escapes raw) true)
;(count-groups (replace-all-escapes raw) false)