(ns aoc2022.day14
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [aoc2022.util :as util]))

(defn draw-rock
  [cave [[xx1 yy1] [xx2 yy2]]]
  (let [x1 (min xx1 xx2)
        x2 (max xx1 xx2)
        y1 (min yy1 yy2)
        y2 (max yy1 yy2)]
    (cond
      (= x1 x2) (reduce (fn [cave y] (assoc cave [x1 y] :rock)) cave (range y1 (inc y2)))
      (= y1 y2) (reduce (fn [cave x] (assoc cave [x y1] :rock)) cave (range x1 (inc x2))))))

(defn parse-line
  [line]
  (map (fn [[_ m1 m2]] (mapv util/str->int [m1 m2])) (re-seq #"(\d+),(\d+)" line)))

(defn ray-down
  [cave x y floor]
  (cond (> y floor) nil
        (cave [x y]) y
        :else (recur cave x (inc y) floor)))

(defn fall-sand
  [cave [x y] floor]
  (let [cand-down (ray-down cave x y floor)
        cand-left [(dec x) cand-down]
        cand-right [(inc x) cand-down]]
    (if (or (nil? cand-down) (= y cand-down)) nil
        (condp #(= (cave %1) %2) nil
          cand-left (recur cave cand-left floor)
          cand-right (recur cave cand-right floor)
          (assoc cave [x (dec cand-down)] :sand)))))

(defn flood-sand
  [cave floor]
  (if-let [cave (fall-sand cave [500 0] floor)]
    (recur cave floor)
    cave))

(defn build-cave
  [filename]
  (->> filename util/load-input
       (map parse-line)
       (map #(partition 2 1 %))
       (apply concat)
       (reduce draw-rock {})))

(defn floor-level
  [cave]
  (+ 2 (reduce (fn [m [[x y] _]] (if (> y m) y m)) 0 cave)))

(defn count-sand
  [cave]
  (count (filter (fn [[_coord val]] (= val :sand)) cave)))

(defn part1
  [filename]
  (let [cave (build-cave filename)
        floor-level (floor-level cave)
        sand (flood-sand cave floor-level)]
    (count-sand sand)))

(defn part2
  [filename]
  (let [cave (build-cave filename)
        floor-level (floor-level cave)
        cave-floor (draw-rock cave [[-999 floor-level] [999 floor-level]])
        sand (flood-sand cave-floor floor-level)]
    (count-sand sand)))

(util/varmista part1 day14_example 24)
(util/varmista part1 day14_input 672)
(util/varmista part2 day14_example 93)
(util/varmista part2 day14_input 26831)
