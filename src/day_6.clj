(ns day_6
  (:require [clojure.string :as str]))

;(def raw (slurp "resources/day_6_test.txt"))
;(def input (str/split (str/trim-newline raw) #"[)]"))

(defn get-file-contents []
  (with-open [reader (clojure.java.io/reader "resources/day_6.txt")]
    (reduce conj [] (line-seq reader))))

(def raw-file-contents (get-file-contents))

(defn generate-parsed-contents [contents]
  (map #(str/split % #"[)]") contents))

(def parsed-file-contents (generate-parsed-contents raw-file-contents))

(def iterables (remove #{"COM"} (set (flatten parsed-file-contents))))

(defn find-starting-point [x] (first (into '()
                                           (filter second (set (map #(if (= (second %) x) % nil) parsed-file-contents))))))

(defn calculate-orbits [sp]
  (loop
    [node (find-starting-point sp)
     node-list '()
     count 1]
    (do
      (println "current node: " node)
      (if (= (first node) "COM")
        [count node-list]
        (let [new-node
              (first (into '()
                           (filter second
                                    (set (map #(if (= (second %) (first node)) % nil)
                                             parsed-file-contents)))))]
          (println count new-node  (first node) node-list)
          (recur new-node (concat node-list (list (first node))) (inc count)))))))

(defn iterate-all []
   (reduce + (map #(calculate-orbits %) iterables)))

(def you (map str (second (calculate-orbits "YOU"))))
(def san (map str (second (calculate-orbits "SAN"))))

(defn determine-distance [you san]
  (-
    (+ (count you) (count san))
    (* (- (+ (count you) (count san))
          (count (set (concat you san)))) 2)))


