(ns utils)
(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn permutations
  "combinations 2x"
  [coll]
  (map #(list (first %)
              (second %)
              (+ (first %)
                 (second %)))
       (let [x (first coll)
             y (next coll)]
         (when y
           (lazy-cat
             (map (fn [z] [x z]) y)
             (all-pairs y))))))

(defn indexes-of
  "retrieves indexs of items in a vector"
  [e coll]
  (keep-indexed #(if (= e %2) %1) coll))