(ns aoc2022.day1
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))

(defn str->int [str] (Integer/parseInt str))

(defn parse
  [filename]
  (-> filename
       io/resource
       slurp
       (str/split #"\n\n")
       (->> (map str/split-lines)
            (map #(map str->int %))
            (map #(apply + %)))))


(defn part1
  [filename]
  (->> filename parse
       (apply max)))

(defn part2
  [filename]
  (->> filename parse
       sort
       (take-last 3)
       (apply +)))

(part1 "day1_part1_example")
(part1 "day1_input")
(part2 "day1_part1_example")
(part2 "day1_input")
