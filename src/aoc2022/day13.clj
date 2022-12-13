(ns aoc2022.day13
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [aoc2022.util :as util]))

(defmacro check-list
  [i1 i2 r1 r2]
  `(let [inner-result# (check-order ~i1 ~i2)]
     (if (= inner-result# :end-reached)
       (recur ~r1 ~r2)
       inner-result#)))

(defn check-order
  [[i1 & r1] [i2 & r2]]
  (match [i1 i2]
    [nil   nil  ] :end-reached
    [nil   _    ] true
    [_     nil  ] false
    [[& _] [& _]] (check-list i1 i2 r1 r2)
    [_     [& _]] (check-list [i1] i2 r1 r2)
    [[& _] _    ] (check-list i1 [i2] r1 r2)
    [_     _    ] (cond (< i1 i2) true
                        (> i1 i2) false
                        (= i1 i2) (recur r1 r2))))

(defn part1
  [filename]
  (->> filename util/load-input
       (remove str/blank?)
       (map read-string)
       (partition 2)
       (map (fn [[l1 l2]] (check-order l1 l2)))
       (keep-indexed (fn [i val] (when val (inc i))))
       (reduce +)))

(defn part2
  [filename]
  (->> filename util/load-input
       (remove str/blank?)
       (map read-string)
       (concat [[[2]] [[6]]])
       (sort check-order)
       (keep-indexed (fn [i val] (when (some #(= val %) [[[2]] [[6]]]) (inc i))))
       (apply *)))

(assert (= (part1 "day13_example") 13))
(assert (= (part1 "day13_input") 5720))
(assert (= (part2 "day13_example") 140))
(assert (= (part2 "day13_input") 23504))

