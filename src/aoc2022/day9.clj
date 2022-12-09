(ns aoc2022.day9
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

(def state1
  {:head {:x 0 :y 0}
   :tail [{:x 0 :y 0}]
   :visited #{{:x 0 :y 0}}})

(def state2
  {:head {:x 0 :y 0}
   :tail (vec (repeat 9 {:x 0 :y 0}))
   :visited #{{:x 0 :y 0}}})

(defn parse-movement
  [line]
  (let [[direction num] (str/split line #" ")
        n (util/str->int num)]
    (repeat n (case direction
                "R" [1 0]
                "L" [-1 0]
                "U" [0 1]
                "D" [0 -1]))))

(defn move
  [state key [x y]]
  (-> state
      (update-in (conj key :x) #(+ % x))
      (update-in (conj key :y) #(+ % y))))

(defn cap
  [x max]
  (cond
    (pos? x) max
    (neg? x) (* -1 max)
    :else 0))

(defn needs-to-move
  [xdif ydif]
  (if (or (> (abs xdif) 1) (> (abs ydif) 1))
    [(cap xdif 1) (cap ydif 1)]
    [0 0]))

(defn tail-follow
  [state headpath tailpath]
  (let [head (get-in state headpath)
        tail (get-in state tailpath)
        xdif (- (:x head) (:x tail))
        ydif (- (:y head) (:y tail))
        [xmove ymove] (needs-to-move xdif ydif)]
    (move state tailpath [xmove ymove])))

(defn add-visited
  [state]
  (update-in state [:visited] #(conj % (last (get state :tail)))))

(defn handle-movement
  [state dir]
  (let [head-moved (move state [:head] dir)
        head-tail-list
        (partition 2 1 (conj (map-indexed (fn [i _] [:tail i]) (:tail head-moved))
                        [:head]))
        tails-moved (reduce (fn [state [head tail]]
                             (tail-follow state head tail))
                           head-moved
                           head-tail-list)]
    (add-visited tails-moved)))

(defn solve
  [filename state]
  (->> filename util/load-input
       (mapcat parse-movement)
       (reduce handle-movement state)
       :visited
       count))

(defn part1
  [filename]
  (solve filename state1))

(defn part2
  [filename]
  (solve filename state2))

(assert (= (part1 "day9_input") 5930))
(assert (= (part1 "day9_example") 13))
(assert (= (part2 "day9_example") 1))
(assert (= (part2 "day9_example_long") 36))
(assert (= (part2 "day9_input") 2443))
