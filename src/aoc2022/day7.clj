(ns aoc2022.day7
  (:require [clojure.string :as str]
            [aoc2022.util :as util]))

(defn update-path
  [pwd cd-command]
  (case cd-command
    "/" ["/"]
    ".." (vec (pop pwd))
    (vec (conj pwd cd-command))))

(defn line->file
  [[size name]]
  {:name name
   :dir (= size "dir")
   :size (if (= size "dir") 0 (util/str->int size))})

(defn parse-line
  [tree lines pwd]
  (if (empty? lines) tree
      (let [parse-line #(str/split % #" ")
            [prefix command & input] (parse-line (first lines))
            rest-of-input (rest lines)]
        (if (= "$" prefix)
          (case command
            "cd" (recur tree rest-of-input (update-path pwd (first input)))
            "ls"
            (let [result-lines (take-while #(not= \$ (first %)) rest-of-input)
                  result (map parse-line result-lines)
                  files (map line->file result)
                  with-children (assoc-in tree [pwd :children] (map :name files))
                  with-new-files (reduce
                                  (fn [tree item] (assoc tree (conj pwd (item :name)) item))
                                  with-children files)]
              (recur with-new-files (drop (count files) rest-of-input) pwd)))
          (recur tree rest-of-input pwd)))))

(defn update-size
  [tree node size]
  (update-in tree [node :size] #(if (nil? %) size (+ % size))))

(defn update-sizes
  "tree tree pop up"
  [tree]
  (reduce
   (fn [tree [key item]]
     (loop [tree tree
            up (pop key)]
       (if (empty? up)
         tree
         (recur
          (update-size tree up (:size item))
          (pop up)))))
   tree tree))

(defn part1
  [filename]
  (->> filename util/load-input
       (#(parse-line {} % []))
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
             (#(parse-line {} % []))
             update-sizes)
        root-size (get-in tree [["/"] :size])
        free (- 70000000 root-size)
        needed (- 30000000 free)]
    (->> tree (map second)
         (filter :dir)
         (filter #(>= (:size %) needed))
         (sort-by :size)
         (map :size)
         (first))))

(util/varmista part1 day7_example 95437)
(util/varmista part1 day7_input 1648397)
(util/varmista part2 day7_example 24933642)
(util/varmista part2 day7_input 1815525)
