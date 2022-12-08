(ns aoc2022.day8
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

(defn transpose
  [matrix]
  (vec (apply map list matrix)))

(defn parse-row
  [line]
  (vec (map #(Character/digit % 10) line)))

(defn is-visible
  [row rowr x]
  (let [val (nth row x)
        watch #(some (partial < val) (take x %))]
    (not (and (watch row) (watch rowr)))))

(defn can-see
  [stack height n]
  (if (empty? stack) n
      (let [n (inc n)]
        (if (>= (first stack) height) n
            (recur (rest stack) height n)))))

(defn scenic-score
  [row rowr x]
  (let [val (nth row x)
        watch #(can-see (take x %) val 0)]
    (* (watch row) (watch rowr))))

(defn matrix-loop
  [fun combiner data]
  (let [cols (transpose data)]
    (for [x (range 0 (count (first data)))
          y (range 0 (count data))]
      (let [row (nth data y)
            rowr (reverse row)
            col (nth cols x)
            colr (reverse col)]
        (future (combiner (fun row rowr x) (fun col colr y)))))))

(defn part1
  [filename]
  (->> filename util/load-input
       (map parse-row)
       (matrix-loop is-visible #(or %1 %2))
       (filter deref)
       count))

(defn part2
  [filename]
  (->> filename util/load-input
       (map parse-row)
       (matrix-loop scenic-score *)
       (map deref)
       (apply max)))

(assert (part1 "day8_example") 21)
(assert (part1 "day8_input") 1705)
(assert (part2 "day8_example") 8)
(assert (part2 "day8_input") 371200)
