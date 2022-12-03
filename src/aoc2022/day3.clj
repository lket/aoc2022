(ns aoc2022.day3
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [aoc2022.util :as util]))

(defn priority
  [char]
  (let [str->char (comp int first)]
    (if (< (int char) (str->char "a")) (- (int char) 38) (- (int char) 96))))

(defn find-common
  [seq]
  (let [len (int (/ (count seq) 2))]
    (first (apply set/intersection (map set (split-at len seq))))))

(defn find-id-in-group
  [group]
    (first (apply set/intersection (map set group))))

(defn part1
  [filename]
  (->> filename util/load-input
       (map seq)
       (map find-common)
       (map priority)
       (reduce +)))

(defn part2
  [filename]
  (->> filename util/load-input
       (map seq)
       (partition 3)
       (map find-id-in-group)
       (map priority)
       (reduce +)))

(part1 "day3_input")
(part2 "day3_input")
