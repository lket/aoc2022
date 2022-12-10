(ns aoc2022.day10
  (:require [clojure.string :as str]
            [aoc2022.util :as util]
            [clojure.core.async :as a :refer [>! <! >!! <!! go go-loop chan buffer close!]]))

(def initial-state
  {:x 1})

(defn addx
  [state n]
  [state (update state :x #(+ % n))])

(defn do-instruction
  [state [op args]]
  (case op
    :noop [state]
    :addx (addx state (first args))))

(defn parse-line
  [line]
  (let [[op & args] (str/split line #" ")
        op (keyword op)
        args (map util/str->int args)]
    [op args]))

(defn cpu
  [input state out]
  (go (>! out state)
      (loop [input input
             state state]
        (if (seq input)
          (let [state-stack (do-instruction state (first input))]
            (doseq [state state-stack]
              (>! out state))
            (recur (rest input) (last state-stack)))
          (a/close! out)))))

(defn want-sycles
  ([] (want-sycles 20))
  ([n]
   (lazy-seq (cons n (want-sycles (+ n 40))))))

(defn monitor
  [input width height]
  (go-loop [cursor 0
            line 0
            output ""]
    (if (>= line height) output
        (if-let [sprite-x (:x (<! input))]
          (let [sprite? (>= 1 (abs (- sprite-x cursor)))
                pixel (if sprite? \# \.)
                newline? (>= cursor 39)]
            (recur
             (mod (inc cursor) 40)
             (if newline? (inc line) line)
             (str output pixel (when newline? \newline))))
          output))))

(defn part1
  [filename]
  (let [cpu-out (chan)
        input (->> filename util/load-input (map parse-line))
        cpu-chan (cpu input initial-state cpu-out)
        record (<!! (a/into [] cpu-out))
        time (count record)
        cycles (map
                #(vector % (nth record (dec %)))
                (take-while #(<= % time) (want-sycles)))]
    (->> cycles
         (map (fn [[n {x :x}]] (* n x)))
         (reduce +))))

(defn part2
  [filename]
  (let [cpu-out (chan)
        input (->> filename util/load-input (map parse-line))
        cpu-chan (cpu input initial-state cpu-out)
        monitor-chan (monitor cpu-out 40 6)]
    (<!! monitor-chan)))

(assert (= (part1 "day10_example") 0))
(assert (= (part1 "day10_example_long") 13140))
(assert (= (part1 "day10_input") 17940))
(assert (= (part2 "day10_example_long")
"##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
"))

(assert (= (part2 "day10_input")
"####..##..###...##....##.####...##.####.
...#.#..#.#..#.#..#....#.#.......#....#.
..#..#....###..#..#....#.###.....#...#..
.#...#....#..#.####....#.#.......#..#...
#....#..#.#..#.#..#.#..#.#....#..#.#....
####..##..###..#..#..##..#.....##..####.
"))
