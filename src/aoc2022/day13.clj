(ns aoc2022.day13
  (:require [clojure.string :as str]
            [aoc2022.util :as util]))

(defn check-order
  [p1 p2]
  (let [i1 (peek p1)
        i2 (peek p2)
        r1 (if (seq p1) (pop p1) nil)
        r2 (if (seq p2) (pop p2) nil)]
    (cond
      (= i1 i2 nil) :end-reached
      (nil? i1) true
      (nil? i2) false
      (or (seqable? i1) (seqable? i2))
      (let [i1 (if (seqable? i1) (vec (rseq i1)) [i1])
            i2 (if (seqable? i2) (vec (rseq i2)) [i2])
            inner-result (check-order i1 i2)]
        (if (= inner-result :end-reached) (recur r1 r2) inner-result))
      (< i1 i2) true
      (= i1 i2) (recur r1 r2)
      (> i1 i2) false)))

(defn part1
  [filename]
  (->> filename util/load-input
       (remove str/blank?)
       (map read-string)
       (map rseq)
       (map vec)
       (partition 2)
       (map (fn [[l1 l2]] (check-order l1 l2)))
       (map #(vector (inc %1) %2) (range))
       (filter (fn [[_ val]] (= true val)))
       (map first)
       (reduce +)))

(defn part2
  [filename]
  (->> filename util/load-input
       (remove str/blank?)
       (map read-string)
       (map rseq)
       (map vec)
       (concat [[[2]] [[6]]])
       (sort check-order)
       (map #(vector (inc %1) %2) (range))
       (reduce (fn [r [i v]] (if (or (= v [[6]]) (= v [[2]])) (conj r i) r)) [])
       (apply *)))

(assert (= (part1 "day13_example") 13))
(assert (= (part1 "day13_input") 5720))
(assert (= (part2 "day13_example") 140))
(assert (= (part2 "day13_input") 23504))
