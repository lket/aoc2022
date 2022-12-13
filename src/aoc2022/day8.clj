(ns aoc2022.day8
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [aoc2022.util :as util]))

(defn transpose
  [matrix]
  (apply mapv vector matrix))

(defn parse-row
  [line]
  (mapv #(Character/digit % 10) line))

(defn is-visible
  [row _ x]
  (let [val (nth row x)
        watch_left #(some (partial <= val) (take x %))
        watch_right #(some (partial <= val) (drop (inc x) %))]
    (not (and (watch_left row) (watch_right row)))))

(defn can-see
  [stack height n]
  (if (empty? stack) n
      (let [n (inc n)]
        (if (>= (peek stack) height) n
            (recur (pop stack) height n)))))

(defn scenic-score
  [row rowr x]
  (let [val (nth row x)
        watch-left #(can-see (vec (take x %)) val 0)
        watch-right #(can-see (vec (take (- (count %) x 1) %)) val 0)]
    (* (watch-left row) (watch-right rowr))))

(defn matrix-loop
  [fun combiner data]
  (let [cols (transpose data)]
    (for [y (range 0 (count data))]
      (let [row (nth data y)
            rowr (rseq row)]
        (for [x (range 0 (count (first data)))]
          (let [col (nth cols x)
                colr (rseq col)]
            (combiner (fun row rowr x) (fun col colr y))))))))

(defn part1
  [filename]
  (->> filename util/load-input
       (map parse-row)
       (matrix-loop is-visible #(or %1 %2))
       (apply concat)
       (filter identity)
       count))

(defn part2
  [filename]
  (->> filename util/load-input
       (map parse-row)
       (matrix-loop scenic-score *)
       (apply concat)
       (apply max)))

(util/varmista part1 day8_example 21)
(util/varmista part1 day8_input 1705)
(util/varmista part2 day8_example 8)
(util/varmista part2 day8_input 371200)

