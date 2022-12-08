(ns aoc2022.day8
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

(def transpose (partial apply map list))

(defn parse-row
  [line]
  (map #(Character/digit % 10) line))

(defn is-visible
  [row x]
  (let [val (nth row x)]
    (not (and
          (some #(>= % val) (take x row))
          (some #(>= % val) (drop (inc x) row))))))

(defn can-see
  [stack height n]
  (if (empty? stack) n
      (let [n (inc n)]
        (if (>= (first stack) height)
          n
          (recur (rest stack) height n)))))

(defn scenic-score
  [row x]
  (let [val (nth row x)]
    (* (can-see (take x row) val 0)
       (can-see (reverse (drop (inc x) row)) val 0))))

(defn matrix-loop
  [fun combiner data]
  (let [cols (transpose data)]
    (for [x (range 0 (count (first data)))
          y (range 0 (count data))]
      (let [row (nth data y)
            col (nth cols x)]
        (combiner (fun row x) (fun col y))))))

(defn part1
  [filename]
  (->> filename util/load-input
       (map parse-row)
       (matrix-loop is-visible #(or %1 %2))
       (filter identity)
       count))

(defn part2
  [filename]
  (->> filename util/load-input
       (map parse-row)
       (matrix-loop scenic-score *)
       (apply max)))

(assert (part1 "day8_example") 21)
(assert (part1 "day8_input") 1705)
(assert (part2 "day8_example") 8)
(assert (part2 "day8_input") 371200)
