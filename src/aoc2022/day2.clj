(ns aoc2022.day2
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

(defn str->charnum
  [str]
  (int (first str)))

; Kivi X, paperi Y, sakset Z
(defn xyz->int
  [char]
  (- (str->charnum char) 87))

; Kivi A, paperi B, sakset Z
(defn abc->int
  [char]
  (- (str->charnum char) 64))

(defn battle-result
  [op me]
  (if (= (abc->int op) (xyz->int me))
    3
    (case op
      "A" (if (= me "Y") 6 0)
      "B" (if (= me "Z") 6 0)
      "C" (if (= me "X") 6 0))))

(defn plus-wrap
  [wrap x plus]
  (inc (mod (+ x plus -1) wrap)))

(defn select-action
  [op w]
  (let [op (abc->int op)
        wrap (partial plus-wrap 3)]
    (case w
      "X" [(wrap op 2) 0]
      "Y" [op 3]
      "Z" [(wrap op 1) 6])))

(defn part1
  [filename]
  (->> filename util/load-input
       (reduce (fn [result line] (let [[op me] (str/split line #" ")]
                                   (+ result (battle-result op me) (xyz->int me)))) 0)))

(defn part2
  [filename]
  (->> filename util/load-input
       (reduce (fn [result line] (let [[op me] (str/split line #" ")
                                       [action-points result-points] (select-action op me)]
                                   (+ result action-points result-points))) 0)))

(part1 "day2_part1_example")
(part1 "day2_input")
(part2 "day2_part1_example")
(part2 "day2_input")
