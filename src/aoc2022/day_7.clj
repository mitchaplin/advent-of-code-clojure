(ns aoc2022.day-7
  (:require [clojure.string :as str]))

(def raw (utils/read-file "resources/2022/day_7.txt"))

(def processed
  (->> raw
       (map #(str/split % #" "))))

(defn update-file-sys
  [fs curr-dir-keymap dir-key value is-dir?]
  (let [new-dir-keymap (if is-dir? (conj curr-dir-keymap dir-key) curr-dir-keymap)]
    (assoc fs
      new-dir-keymap
      (if (nil? (get fs new-dir-keymap))
        value
        (+ value (get fs new-dir-keymap))))))

(defn execute-instruction
  [l curr-dir-keymap fs]
  (cond (> (count l) 2)
        (cond (= (last l) "..")
              [(vec (butlast curr-dir-keymap)) fs]

              (= (last l) "/")
              [(vector (first curr-dir-keymap)) fs]

              :else
              [(conj curr-dir-keymap (keyword (last l))) fs])
        (= (count l) 2)
        (cond (= (first l) "dir")
              [curr-dir-keymap (update-file-sys fs curr-dir-keymap (keyword (second l)) 0 true)]

              (= (second l) "ls")
              [curr-dir-keymap fs]

              :else
              [curr-dir-keymap (update-file-sys fs curr-dir-keymap (last curr-dir-keymap) (utils/parse-int (first l)) false)])))

(defn run-instructions
  []
  (loop [i processed
         curr-dir-keymap [:/]
         file-sys {}]
    (if (empty? i)
      (sort file-sys)
      (let [[km fs] (execute-instruction (first i) curr-dir-keymap file-sys)]
        (recur (rest i)
               km
               fs)))))

(def sorted-fs (run-instructions))
(def fs-keys (map key sorted-fs))
(def total-map (->> fs-keys
                    (map (fn [x] {x (->> (filter #(utils/col-starts-with? (first %) x) sorted-fs)
                                         (map second)
                                         (reduce +))}))
                    (apply merge)))

(defn part-1
  []
  (->> total-map
       (remove #(> (second %) 100000))
       (vals)
       (reduce +)))

(defn part-2
  []
  (->> total-map
       (filter #(> (second %) (- 30000000 (- 70000000 (get total-map [:/])))))
       (sort-by second)
       first
       (val)))
