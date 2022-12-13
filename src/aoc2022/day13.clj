(ns aoc2022.day13
  (:require [clojure.string :as str]
            [aoc2022.util :as util]))

(defn check-order
  [items]
  (if (seq items) 
    (let [[i1 i2] (peek items)
          remaining (if (seq items) (pop items) nil)]
      (println i1 i2)
      (cond
        (= i1 i2 ##-Inf) (recur remaining)
        (= i1 ##-Inf) true
        (= i2 ##-Inf) false
        (or (seqable? i1) (seqable? i2))
        (let [i1 (if (seqable? i1) (if (seq i1) i1 [##-Inf]) [i1 ##-Inf])
              i2 (if (seqable? i2) (if (seq i2) i2 [##-Inf]) [i2 ##-Inf])
              inner-result (check-order (vec (rseq (mapv #(vector %1 %2) i1 i2))))]
          (if (= inner-result :end-reached) (recur remaining) inner-result))
        (< i1 i2) true
        (= i1 i2) (recur remaining)
        (> i1 i2) false)) :end-reached))


(check-order '((1 1)))

(defn part1
  [filename]
  (->> filename util/load-input
       (remove str/blank?)
       (map read-string)
       (map seq)
       (partition 2)
       (mapv (fn [[p1 p2]] (mapv #(vector %1 %2) (concat p1 [##-Inf]) (concat p2 [##-Inf]))))
       (mapv (comp vec rseq))
       (map check-order)
       (map #(vector (inc %1) %2) (range))
       (filter (fn [[_ val]] (= true val)))
       (map first)
       (reduce +)))


(part1 "day13_example")
(part1 "day13_input")
(part1 "day13_test")
(seqable? [[]])
(seq? [])


(concat [1 2] '(1 3))

(mapv #(vector %1 %2) (rseq [2 3 4]) [4 ##-Inf])
(mapv #(vector %1 %2) (rseq [1 2 3]) [5 4])
(mapv #(vector %1 %2) [[]] [##-Inf])
