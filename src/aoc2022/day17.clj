(ns aoc2022.day17
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [aoc2022.util :as util]))
(def vaaka [(vector-of :byte 1 1 1 1)])
(def risti [(vector-of :byte 0 1 0)
            (vector-of :byte 1 1 1)
            (vector-of :byte 0 1 0)])
(def l     [(vector-of :byte 1 1 1)
            (vector-of :byte 0 0 1)
            (vector-of :byte 0 0 1)])
(def i     [(vector-of :byte 1)
            (vector-of :byte 1)
            (vector-of :byte 1)
            (vector-of :byte 1)])
(def ruutu [(vector-of :byte 1 1)
            (vector-of :byte 1 1)])

(def tetris-pattern (cycle [vaaka risti l i ruutu :pattern-end]))
(def full-row [1 1 1 1 1 1 1])

(defn parse-wind-pattern
  [line]
  (cycle (conj (map #(if (= % \>) :right :left) line) :wind-end)))

(defn check-collisions
  [state shape x y width height]
  (cond (or (not (< -1 x 7)) (< 7 (+ x width)) (< y 0)) false
        (< (inc (count state)) y) true
        :else (reduce (fn [result [wx hy]]
                        (let [shape-val (get-in shape [hy wx])
                              state-val (get-in state [(+ hy y) (+ wx x)] 0)
                              fits (and (< (+ wx x) 7) (< (+ shape-val state-val) 2))]
                          (if (not fits) (reduced false) true))) false
                      (for [wx (range 0 width)
                            hy (range 0 height)] [wx hy]))))

(def check-collisions-m (memoize check-collisions))

(defn check-move
  [state shape x y move width height]
  (let [[new-x new-y] (p :case (case move
                                 :left [(dec x) y]
                                 :right [(inc x) y]
                                 :down [x (dec y)]))
        valid? (p :check (check-collisions state shape new-x new-y width height))]
    (if valid? [new-x new-y] [x y])))

(defn tetris-merge
  [state shape x y]
  (let [height (count shape)
        width (count (first shape))]
    (reduce (fn [[state growth tetris-n] [wx hy]]
              (let [height-needed (+ y hy 1)
                    state-size (count state)
                    growth (if (< state-size height-needed) (+ growth (- height-needed state-size)) growth)
                    new-state (if (< state-size height-needed)
                                (into state (repeat (- height-needed state-size)
                                                    (vector-of :byte 0 0 0 0 0 0 0)))
                                state)
                    new-state (update-in new-state [(+ y hy) (+ x wx)]
                                         #(+ (if (nil? %) 0 %) (^long get-in shape [hy wx] 0)))
                    tetris? (= (get new-state (+ y hy)) full-row)
                    tetris-n (if tetris? (+ y hy 1) tetris-n)]
                [new-state growth tetris-n])) [state 0 0]
            (for [wx (range 0 width)
                  hy (range 0 height)]
              [wx hy]))))

(defn fall-down
  [tetris tetris-pattern move-pattern tetris-height n pattern-n]
  (if (< n pattern-n) tetris-height
      (let [pattern-magic? (= (first tetris-pattern) :pattern-end)
            tetris-pattern (if pattern-magic? (rest tetris-pattern) tetris-pattern)
            shape (first tetris-pattern)
            height (count shape)
            width (count (first shape))
            [tetris move-pattern tetris-height]
            (loop [x 2
                   y (+ 3 (count tetris))
                   state tetris
                   move-pattern move-pattern
                   magic-moment? false]
              (let [move-magic? (= (first move-pattern) :wind-end)
                    move-pattern (if move-magic? (rest (rest move-pattern)) move-pattern)
                    move (first move-pattern)
                    [new-x new-y] (check-move state shape x y move width height)
                    magic-moment? (or magic-moment? (and (= :pattern-end (first (rest tetris-pattern)))
                                                         (= :wind-end (first (rest move-pattern)))))]
                (if (and (= move :down) (= y new-y))
                  (let [[fallen-state growth tetris-n] (p :merge (tetris-merge state shape x y))]
                    (when magic-moment?
                      (println "MAGIC TIME? " (+ tetris-height growth) pattern-n (first (rest (rest tetris-pattern))) (first (rest move-pattern)) (last fallen-state)))
                    [(vec (drop tetris-n fallen-state)) (rest move-pattern) (+ tetris-height growth)])
                  (recur new-x new-y state (rest move-pattern) magic-moment?))))]
        (recur tetris (rest tetris-pattern) move-pattern tetris-height n (inc pattern-n)))))

(defn solve
  [filename n]
  (let [input-pattern (-> filename util/load-input first)
        move-pattern (interleave
                      (parse-wind-pattern input-pattern)
                      (repeat :down))]
    (fall-down [] tetris-pattern move-pattern 0 n 0)))

;(println "!!!!!!!!!!!!")
;(pp/pprint (count (solve "day17_input" 1000000000000N)))
(pp/pprint (solve "day17_example" 1))
(pp/pprint (solve "day17_example" 189))
(pp/pprint (solve "day17_example" 190))
(pp/pprint (solve "day17_example" 12))
(pp/pprint (solve "day17_input" 20210))
(pp/pprint (solve "day17_input" 189))
(pp/pprint (solve "day17_input" (+ 19194 1010)))
(- 304399 30529)
(- 32155 30529)
(- 32154 30529)
(pp/pprint (solve "day17_input" (+ 19194 996)))
(pp/pprint (solve "day17_input" 191200))
(- 1893 293)
(- 32130 30529)
(pp/pprint (solve "day17_input" 20210))
;(pp/pprint (solve "day17_example" (dec 1000000000000)))
;(pp/pprint (solve "day17_input" (dec 1000000000000)))
;(subvec)
;(first tetris-pattern)
;
;(dec 1000000000000N)
;(vector-of)
;(tufte/add-basic-println-handler! {})

(def target 1000000000000)

(defn find-smaller
  [x]
  (+ 1744 (* 1745 (bigint (/ (- x 1744) 1745)))))

(defn calc-loop-point
  [x]
  (+ 2749 (* 2778 (/ (- x 1744) 1745))))

(defn calc-value
  [x]
  (let [smaller (find-smaller x)
        diff (- x smaller)]
    [(bigint (calc-loop-point smaller)) diff]))

(defn find-smaller
  [x]
  (+ 14 (* 35 (bigint (/ x 35)))))

(defn calc-loop-point
  [x]
  (+ 25 (* 53 (/ (- x 14) 35))))

(* 35 (int (/ 200 35)))
(int (mod 200 35))

(int (/ 189 35))
(int (mod 189 35))

(- target 14)

(int (calc-loop-point 12))

(- 201 189)
290
(calc-loop-point (find-smaller target))
(calc-loop-point (find-smaller 189))
(calc-value 190)
(calc-value target)
(calc-loop-point (find-smaller target))
(find-smaller 190)
(find-smaller target)

(find-smaller  19195)

(println "TARGET CALC " (calc-value target))
(println (+ 1591977075727N 1626))
(println (+ 1591977075727N 1625))
(println (calc-value 191200))
(println (+ 302773 1601))
(+ 1600 (first (calc-value target)))
()

(* 1745 (bigint (/ 1746 1745)))
