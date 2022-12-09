(ns aoc2022.day9
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

(def state
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
                "R" :right
                "L" :left
                "U" :up
                "D" :down))))

(defn move
  [state dir key]
  (case dir
    :right (update-in state [key :x] inc)
    :left (update-in state [key :x] dec)
    :up (update-in state [key :y] inc)
    :down (update-in state [key :y] dec)))

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
    (-> state
        (update-in (conj tailpath :x) #(+ % xmove))
        (update-in (conj tailpath :y) #(+ % ymove)))))

(defn add-visited
  [state tail]
  (update-in state [:visited] #(conj % (get-in state tail))))

(defn handle-movement
  [state dir]
  (let [headmoved (move state dir :head)
        tailmoved (reduce (fn [state tail-n]
                            (let [follow (if (zero? tail-n) [:head] [:tail (dec tail-n)])]
                              (tail-follow state follow [:tail tail-n])))
                          headmoved
                          (range 0 (count (:tail headmoved))))]
    (add-visited tailmoved [:tail 8])))

(defn part1
  [filename]
  (->> filename util/load-input
       (mapcat parse-movement)
       (reduce handle-movement state)
       :visited
       count))

(defn part2
  [filename]
  (->> filename util/load-input
       (mapcat parse-movement)
       (reduce handle-movement state2)
       :visited
       count))

(assert (part1 "day9_input") 5930)
(assert (part1 "day9_example") 13)
(assert (part2 "day9_example") 1)
(assert (part2 "day9_example_long") 36)
(assert (part2 "day9_input") 2443)
