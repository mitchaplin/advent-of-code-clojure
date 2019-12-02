(def input [1 12 2 3 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 19 1 19 9 23 1 5 23 27 1 27 9 31 1 6 31 35 2 35 9 39 1 39 6 43 2 9 43 47 1 47 6 51 2 51 9 55 1 5 55 59 2 59 6 63 1 9 63 67 1 67 10 71 1 71 13 75 2 13 75 79 1 6 79 83 2 9 83 87 1 87 6 91 2 10 91 95 2 13 95 99 1 9 99 103 1 5 103 107 2 9 107 111 1 111 5 115 1 115 5 119 1 10 119 123 1 13 123 127 1 2 127 131 1 131 13 0 99 2 14 0 0])

(defn add [first second] (+ first second))
(defn mult [first second] (* first second))

(defn determine-op [num] (if (= num 1) add mult))

(defn grav-assist
  [input]
  (get (loop [iter 0 arr input]
         (if (or (> iter (- (count arr) 1)) (= (get (subvec arr (+ iter 0) (+ iter 1)) 0) 99))
           arr
           (recur (+ iter 4)
                  (assoc arr (get arr (+ iter 3) 0)
                             ((determine-op (get arr iter))
                              (get arr (get (subvec arr (+ iter 1) (+ iter 2)) 0))
                              (get arr (get (subvec arr (+ iter 2) (+ iter 3)) 0))))))) 0))

(defn determine-input
  [input grav-assist]
  (loop [noun 0 verb 0 output 0 inp input]
    (if (= output 19690720)
      inp
      (do
        (println noun verb output)
        (if (>= verb 100)
          (recur (inc noun) 0 (grav-assist inp) (assoc inp 1 noun))
          (recur noun (inc verb) (grav-assist inp) (assoc inp 2 verb)))))))

