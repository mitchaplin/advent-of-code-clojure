(ns aoc2021.day_14
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2021/day_14.txt"))
(def processed (remove #(= (first % ) "") (map #(str/split % #" ") (str/split-lines raw))))

(defn assign-polymer-mappings
  [p]
  (let [[pair _ insertion] p]
    {pair insertion}))

(defn create-polymer-mappings
  []
  (into {} (map assign-polymer-mappings (rest processed))))

(def polymer-mappings (create-polymer-mappings))

(defn build-polymer
  [polymer]
  (let [polymer-string polymer
        part (partition 2 1 polymer-string)
        str-part (map #(apply str %) part)
        pm polymer-mappings
        test (apply str
                    (apply concat
                           (map rest
                                (map (fn [x] (str (first x) (get pm x)
                                                  (str (last x)))) str-part))))
        final (str (first polymer-string) test)]
    final))

;PART 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defn run
  []
  (->> (ffirst processed)
    (iterate build-polymer)
    (take 41)
    last
    frequencies))

(defn p1
  []
  (- (apply max (vals (run))) (apply min (vals (run)))))

;PART 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn expand-polymer
  [form amount]
  (let [found (get polymer-mappings form)]
    {(str (first form) found) amount
     (str found (second form)) amount}))

(defn get-polymer-frequencies
  [polymer-arg]
  (let [[p f] polymer-arg]
    (loop [pol p
           freq f
           acc {}]
      (if (empty? pol)
        [acc freq]
        (let [[form amount] (first pol)
              found (get polymer-mappings form)
              next-polymer-freq (expand-polymer form amount)]
          (recur (rest pol)
                 (merge-with + freq {found amount})
                 (merge-with + acc next-polymer-freq)))))))

(defn p2
  []
  (let [v (second (first (->> (iterate get-polymer-frequencies [(frequencies (map #(apply str %)
                                                                                  (partition 2 1 "PPFCHPFNCKOKOSBVCFPP")))
                                                                (frequencies (map str "PPFCHPFNCKOKOSBVCFPP"))])
                              (drop 40)
                              (take 1))))
        minimum (second (apply min-key val v))
        maximum (second (apply max-key val v))]
    (- maximum minimum)))