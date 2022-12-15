(ns aoc2022.day15
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clj-async-profiler.core :as prof]
            [aoc2022.util :as util]))

(defn add-range
  [[ s1 e1] [s2 e2]]
  (cond
    (<= s1 s2 (inc e1) e2) [[s1 e2]]
    (<= s1 s2 e2 e1) [[s1 e1]]
    (<= s1 e1 s2 e2) [[s1 e1] [s2 e2]]))

(defn simplify-range
  [range]
  (let [range (sort range)]
    (reduce (fn [complete new]
              (let [last (peek complete)]
                (if last 
                  (into (pop complete) (add-range last new))
                  [new]))) [] range)))

(defn dis
  [x1 y1 x2 y2]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn parse-line
  [line]
  (let [[[_ x1 y1] [_ x2 y2]] (re-seq #"x=(-?\d+), y=(-?\d+)" line)
        [x1 y1 x2 y2] (map util/str->int [x1 y1 x2 y2])]
    [x1 y1 (dis x1 y1 x2 y2)]))

(defn possibilities
  [row sensors]
  (reduce (fn [covered [x y d]]
            (let [row-dis (abs (- row y))
                  left (- d row-dis)]
              (if (pos? left)
                (simplify-range (conj covered [(- x left) (+ x left)]))
                covered)))
          [] sensors))

(defn cover
  [[space minn maxx] [x y d]]
  (if (and (> maxx (- x d)) (> maxx (- y d)))
    (reduce (fn [[space minn maxx] line]
              (let [row-denied (get space line [])
                    row-dis (abs (- line y))
                    left (- d row-dis)
                    [[s e] :as range] (simplify-range (conj row-denied [(- x left) (+ x left)]))
                    full? (< s minn maxx e)
                    minn (if (and full? (= line minn)) (inc minn) minn)]
                [(assoc space line range) minn maxx])
              ) [space minn maxx] (range (max minn (- y d)) (min maxx (+ y d)))) [space minn maxx]))

(defn part1
  [filename line]
  (->> filename util/load-input
       (map parse-line)
       (possibilities line)
       ((fn [[[s e]]] (abs (- s e))))))

(defn part2
  [filename min max]
  (->> filename util/load-input
       (map parse-line)
       (sort-by :y)
       (reduce cover [{} min max])
       first
       (filter (fn [[i [[s e]]]] (and (<= min i max) (or (> s min) (< e max)))))
       ((fn [[[i [[_ e] [_ _]]]]] (+ i (* (inc e) 4000000))))))


(part1 "day15_example" 10)
(part2 "day15_example" 0 20)
;(time (part2 "day15_input" 0 4000000))
