(ns aoc2022.day6
  (:require [clojure.string :as str]
            [aoc2022.util :as util]))

(defn find-packet
  [stream n buffer window]
  (let [top (peek stream)
        diff (= window (count (set buffer)))]
    (if diff n
        (recur (pop stream) (inc n) (cons top (take (dec window) buffer)) window))))

(defn part1
  [filename]
  (-> filename util/load-input
      first
      str/reverse
      vec
      (find-packet 0 [] 4)))

(defn part2
  [filename]
  (-> filename util/load-input
      first
      str/reverse
      vec
      (find-packet 0 [] 14)))

(util/varmista part1 day6_input 1531)
(util/varmista part2 day6_input 2518)
