(ns aoc2022.day7
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

(defn combine-dir
  [path name]
  (let [full-path (str path (if (and (not= (first name) \/) (not= (last path) \/)) "/" "") name)] 
    full-path))

(defn parse-line
  [tree lines pwd]
  (if (empty? lines) tree 
      (let [parse-line #(str/split % #" ")
            next-input (parse-line (first lines))
            rest-input (rest lines)]
        (if (= "$" (first next-input))
          (case (second next-input)
            "cd" (recur tree
                        rest-input
                        (let [dir (nth next-input 2)]
                                          (if (= dir "..")
                                            (get-in tree [pwd :up])
                                            (combine-dir pwd dir))))
            "ls" (let [result (take-while #(not= \$ (first %)) rest-input)
                       result-lines (map parse-line result)
                       files (reduce (fn [resulttree item]
                                       (conj resulttree
                                             {:name (combine-dir pwd (second item)) :up pwd :dir (= (first item) "dir") :size (if (= (first item) "dir") 0 (util/str->int (first item)))}))
                                     [] result-lines)]
                   (recur (reduce (fn [tree item]
                                    (assoc tree (get item :name) item))
                                  (assoc-in tree [pwd :children] (map :name files)) files)
                          (drop (count files) rest-input) pwd)))
          (recur tree rest-input pwd)))))

(defn update-size
  [tree node size]
  (update-in tree [node :size] #(if (nil? %) size (+ % size))))

(defn update-sizes
  [tree]
  (reduce (fn [tree pair]
            (let [item (second pair)]
              (loop [tree tree
                     key (:up item)]
                (if key (recur
                         (update-size tree key (:size item))
                         (get-in tree [key :up]))
                    tree))
              )) tree tree))

(defn part1
  [filename]
  (->> filename util/load-input
       (#(parse-line {} % ""))
       update-sizes
       (map second)
       (filter #(<= (:size %) 100000))
       (filter :dir)
       (map :size)
       (reduce +)))

(defn part2
  [filename]
  (let [tree 
        (->> filename util/load-input
             (#(parse-line {} % ""))
             update-sizes)
        root-size (get-in tree ["/" :size])
        free (- 70000000 root-size)
        needed (- 30000000 free)]
    (->> tree (map second)
         (filter :dir)
         (filter #(>= (:size %) needed))
         (sort-by :size)
         (map :size)
         (first))))


(part1 "day7_example")
(part1 "day7_input")
(part2 "day7_example")
(part2 "day7_input")

