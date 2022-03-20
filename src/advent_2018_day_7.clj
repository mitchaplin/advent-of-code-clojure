(ns advent_2018_day_7
  (:require [clojure.string :as str]))

(def raw (slurp "resources/2018/day_7.edn"))
(def processed (str/split-lines raw))

(defn add-duration
  [letter]
  (+ 60 (inc (.indexOf (map str/upper-case (map char (range 97 123))) letter))))

(defn line-to-pair
  [line]
  (rest (re-find #"Step (\w) must be finished before step (\w)" line)))

(defn lines-to-pairs
  [lines]
  (map line-to-pair lines))

(defn setup-graph
  [pairs]
  (reduce (fn [graph key]
            (assoc graph key #{}))
          {} (distinct (flatten pairs))))

(defn create-graph
  [pairs]
  (reduce (fn [ret [dep key]]
            (update ret key conj dep))
          (setup-graph pairs) pairs))

(defn initialize-worker-status
  []
  {:1 {:letter   nil
       :duration nil}
   :2 {
       :letter   nil
       :duration nil}
   :3 {
       :letter   nil
       :duration nil}
   :4 {
       :letter   nil
       :duration nil}
   :5 {
       :letter   nil
       :duration nil}})

(defn update-worker-status
  [v]
  (cond
    (= (:duration v) 0)
    (update (update v :duration (constantly nil)) :letter (constantly nil))

    (nil? (:duration v))
    v

    (> (:duration v) 0)
    (update v :duration (constantly (dec (:duration v))))

    :else
    v))

(defn assign-task-to-worker-on-tick
  [task-list worker-status]
  (loop [tl task-list
         ws worker-status]
    (let [_ (println tl)
          break (or (not (some #(nil? (:duration (second %))) ws)) (empty? tl))
          idle-workers (keep #(when (nil? (:duration (second %))) %) ws)]
      (if break
        (list tl ws)
        (recur
          (rest tl)
          (assoc ws (ffirst idle-workers) {:letter (first tl) :duration (add-duration (first tl))}))))))

(defn remove-step
  [graph step]
  (let [graph (dissoc graph step)]
    (reduce (fn [ret key]
              (update ret key disj step))
            graph (keys graph))))

(defn find-next-step
  [graph]
  (let [sortable (sort #(compare (first %1) (first %2))
                       (map #(list (first %) (count (last %))) graph))]
    (sort #(compare (last %1) (last %2)) sortable)))

(defn find-step-order
  [graph]
  (loop [g graph
         steps ()
         tick 0
         ws (initialize-worker-status)]
    (cond
      (empty? g)
      (apply str (reverse steps))

      :else
      (let [available-steps (sort (map first (keep #(when (zero? (second %)) %) (find-next-step g))))
            updated-workers (assign-task-to-worker-on-tick available-steps ws)
            next-steps (sorted-set (concat (first updated-workers) available-steps))
            tick (inc tick)
            removed-steps (sort (clojure.set/difference (set available-steps) next-steps))
            updated-graph (loop [updated-graph g
                                 rs removed-steps]
                            (if (empty? rs)
                              updated-graph
                              (recur
                                (remove-step updated-graph (first rs))
                                (rest rs))))]
        (recur updated-graph next-steps tick (second updated-workers))))))

(defn part-1
  []
  (->> processed
       (lines-to-pairs)
       (create-graph)
       (find-step-order)))

(defn part-2
  []
  (->> processed
       (lines-to-pairs)
       (create-graph)
       (find-step-order)))