(ns aoc2022.day5
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

(defn parse-stack
  [lines]
  (let [letters (map #(map second (partition 4 4 nil %)) (drop-last 1 lines))
        not-blank? #(not= % \space)
        stack (apply map list letters)]
    (map #(filter not-blank? %) stack)))

(defn split-on-empty
  [seq]
  (split-at (.indexOf seq "") seq))

(defn parse-command
  [line]
  (let [input (str/split line #" ")]
    (map #(util/str->int (nth input %)) '(1 3 5))))

(defn pick-crate
  [stacks n from to]
  (let [from (dec from)
        to (dec to)
        items (take n (nth stacks from))]
    (if (nil? items) stacks 
        (map-indexed (fn [i stack] (cond 
                                     (= i from) (drop n stack)
                                     (= i to) (concat items stack)
                                     :else stack)) (doall stacks)))))

(defn move-crates
  [stack n from to]
  (let [new-stack (pick-crate stack 1 from to)] 
    (if (<= n 1) new-stack (recur new-stack (dec n) from to))))

(defn read-command
  [stack input]
  (let [[n from to] (parse-command input)]
    (move-crates stack n from to)))

(defn read-command2
  [stack input]
  (let [[n from to] (parse-command input)]
    (pick-crate stack n from to)))

(defn part1
  [filename]
  (let [[stacks input] (->> filename util/load-input split-on-empty)
        stack (parse-stack stacks)]
    (str/join (map first (reduce read-command stack (drop 1 input))))))

(defn part2
  [filename]
  (let [[stacks input] (->> filename util/load-input split-on-empty)
        stack (parse-stack stacks)]
    (str/join (map first (reduce read-command2 stack (drop 1 input))))))

(part1 "day5_part1_example")
(part1 "day5_input")
(part2 "day5_part1_example")
(part2 "day5_input")
