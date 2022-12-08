(ns aoc2022.day8
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

(defn parse-row
  [line]
  (map #(Character/digit % 10) line))

(defn is-visible-row
  [row x]
  (let [val (nth row x)]
    (not (and
          (some #(>= % val) (take x row))
          (some #(>= % val) (drop (inc x) row))))))


(defn can-see
  [stack height n]
  (if (empty? stack) n 
      (let [n (inc n)]
        (if (>= (peek stack) height)
          n
          (recur (pop stack) height n)))))

(defn scenic-score-row
  [row x]
  (let [val (nth row x)]
    (* (can-see (vec (take x row)) val 0)
       (can-see (vec (reverse (drop (inc x) row))) val 0))))

(scenic-score-row
 [2 5 5 1 2] 2)
(scenic-score-row
 [3 3 5  4 9] 2)
(scenic-score-row
 [3 5 3 5 3] 3)

(defn is-visible
  [data x y]
  (let [row (nth data y)
        col (nth (apply map list data) x)]
    (or (is-visible-row row x)
        (is-visible-row col y))))

(defn scenic-score
  [data x y]
  (let [row (nth data y)
        col (nth (apply map list data) x)]
    (* (scenic-score-row row x)
        (scenic-score-row col y))))

(defn part1
  [filename]
  (->> filename util/load-input
       (map parse-row)
       ((fn [data]
          (for [x (range 0 (count (first data)))
                y (range 0 (count data))]
            (let [visible (is-visible data x y)]
              visible))))
       (filter identity)
       count))

(defn part2
  [filename]
  (->> filename util/load-input
       (map parse-row)
       ((fn [data]
          (for [x (range 0 (count (first data)))
                y (range 0 (count data))]
            (scenic-score data x y))))
       (apply max)
       ))

(part1 "day8_input")
(part2 "day8_input")
(part2 "day8_example")

(nth (util/dbgv (map identity [1 2 3])) 1)
