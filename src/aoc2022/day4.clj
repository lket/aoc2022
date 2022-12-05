(ns aoc2022.day4
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [aoc2022.util :as util]))

(defn compare-pairs
  [[a b] [x y]]
  (and (>= a x) (<= b y)))

(defn compare-pairs-full
  [[a b] [x y]]
  (and (< a x) (< b x)))

(defn parse-pair
  [pair-str]
  (map util/str->int (str/split pair-str #"-")))

(defn part1
  [filename]
  (->> filename util/load-input
       (map #(str/split % #","))
       (map (fn [pair] (map parse-pair pair)))
  (filter (fn [[a b]] (or (compare-pairs a b) (compare-pairs b a))))
  count))

(defn part2
  [filename]
  (->> filename util/load-input
       (map #(str/split % #","))
       (map (fn [pair] (map parse-pair pair)))
       (filter (fn [[a b]] (not (or (compare-pairs-full a b) (compare-pairs-full b a)))))
       count
  ))

(part1 "day4_input")
(part2 "day4_input")
