(ns aoc2022.day-13
  (:require
    [clojure.edn :as edn]
    [clojure.string :as str]))

(def raw (map #(map edn/read-string %) (map #(str/split % #"\n") (utils/split-blank-line (slurp "resources/2022/day_13.txt")))))

(defn is-same
  [remainder]
  (println "REMAINDER" remainder)
  (reduce (fn [acc v]
            (cond (true? v)
                  (reduced true)

                  (false? v)
                  (reduced false)))
          nil remainder))

(defn determine-total
  [left right]
  (println left right)
  (cond
    ;; both are numbers
    (and (number? left) (number? right))
    (cond
      (> right left)
      true

      (< right left)
      false

      :else
      nil)

    ;; both are vectors
    (and (vector? left) (vector? right))
    (let [result (is-same (map determine-total left right))]
      (cond

        ;; nested vectors resulted in sameness
        (true? result)
        true

        ;; nested vectors did not result in sameness
        (false? result)
        false

        ;; once here, result came back as nil therefore we can check counts of vectors
        (< (count left)
           (count right))
        true

        (> (count left)
           (count right))
        false))

    ;; left is number, right is vector -> wrap left in vector
    (number? left)
    (determine-total (vector left) right)

    ;; right is number, left is vector -> wrap right in vector
    (number? right)
    (determine-total left (vector right))))

(defn iterate-through-input
  [input]
  (loop [i input
         total 0
         idx 1]
    (if (empty? i)
      total
      (let [[left right] (first i)
            is-ordered? (determine-total left right)]
        (recur (rest i)
               (if is-ordered? (+ idx total) total)
               (inc idx))))))
