(ns aoc2022.day18
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clj-async-profiler.core :as prof]
            [aoc2022.util :as util]))

(defn neighbours
  [x y z]
  (let [diffs [[-1 0 0] [1 0 0] [0 -1 0] [0 1 0] [0 0 -1] [0 0 1]]]
    (into #{} (map (fn [[dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)]) diffs))))

(defn parse-line
  [line]
  (map util/str->int (rest (re-find #"(\d+),(\d+),(\d+)" line))))

(defn calc-surface
  [voxels]
  (reduce (fn [[n blob] [x y z]]
            (let [neighbours (neighbours x y z)
                  closed-surfaces (count (set/intersection blob neighbours))]
              [(+ n 6 (* -2 closed-surfaces)) (conj blob [x y z])]))
          [0 #{}] voxels))

(defn shoot-ray
  [voxels]
  (let [[x y _] (rand-nth (seq voxels))]
    (loop [z -99]
      (if (voxels [x y z])
        [x y (dec z)]
        (recur (inc z))))))

(defn spread
  [voxels surface potentials rejected exposed-area]
  (let [[x y z :as this] (first potentials) 
        neighbours (neighbours x y z)
        touches (set/intersection neighbours voxels)
        touches? (not-empty touches)
        exposed-area (+ exposed-area (count touches))
        surface (if touches? (conj surface this) surface)
        rejected (if-not touches? (conj rejected this) rejected)
        touches-surface? (not-empty (set/intersection neighbours surface))
        air (set/difference neighbours voxels)
        potentials (if (or touches? touches-surface?) (set/union potentials air) potentials)
        potentials (set/difference potentials surface rejected)]
    (if (empty? potentials) 
      exposed-area
      (recur voxels surface potentials rejected exposed-area))))

(defn part1
  [filename]
  (->> filename util/load-input
       (map parse-line)
       calc-surface
       first))

(defn part2
  [filename]
  (let [voxels (->> filename util/load-input
                    (map parse-line)
                    calc-surface
                    second)
        start-pos (shoot-ray voxels)]
    (spread voxels #{} #{start-pos} #{} 0)))
