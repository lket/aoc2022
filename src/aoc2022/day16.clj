(ns aoc2022.day16
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]
            [aoc2022.util :as util]))

(defn max-possible
  [tree time-left]
  (reduce (fn [possible {rate :rate open? :open}]
            (cond
              open? possible
              :else (+ possible (* rate time-left))))
          0 (vals tree)))

(defn elefanttimarssi
  [tree me elephant m-parent e-parent time-left paths value]
  (if (or
       (< time-left 2)
       (< (+ value (max-possible tree time-left)) paths)) (max paths value)
      (let [e-girl (get tree elephant)
            mie (get tree me)
            m-tunnels (get mie :tunnels)
            m-rate (get mie :rate)
            m-opened (get mie :open)
            m-could-open? (and (not m-opened) (< 0 m-rate))
            m-tunnels (if m-could-open? m-tunnels (remove #(= % m-parent) m-tunnels))
            e-tunnels (get e-girl :tunnels)
            e-rate (get e-girl :rate)
            e-opened (get e-girl :open)
            e-could-open? (and (not e-opened) (< 0 e-rate))
            e-tunnels (if e-could-open? e-tunnels (remove #(= % e-parent) e-tunnels))
            possible-actions (for [me-action (if m-could-open? (into [:open] m-tunnels) m-tunnels)
                                   elephant-action (if e-could-open? (into [:open] e-tunnels) e-tunnels)]
                               [me-action elephant-action])]
        (if (seq possible-actions)
          (reduce (fn [paths [me-action elephant-action]]
                    (if (and (= me elephant) (= :open me-action elephant-action)) paths
                        (let [tree (if (= me-action :open)
                                     (assoc-in tree [me :open] true) tree)
                              tree (if (= elephant-action :open)
                                     (assoc-in tree [elephant :open] true) tree)
                              m-tunnel (if (= me-action :open) me me-action)
                              e-tunnel (if (= elephant-action :open) elephant elephant-action)
                              value (if (= me-action :open) (+ value (* m-rate (dec time-left))) value)
                              value (if (= elephant-action :open) (+ value (* e-rate (dec time-left))) value)]
                          (elefanttimarssi tree m-tunnel e-tunnel me elephant (dec time-left) paths value))))
                  paths possible-actions)
          (max paths value)))))

(defn walk-tree
  [tree id time-left parent paths value]
  (if (or
       (< time-left 2)
       (< (+ value (max-possible tree time-left)) paths)
       (< 4 (get-in tree [id :visited] 0))) (max paths value)
      (let [tunnels (get-in tree [id :tunnels])
            rate (get-in tree [id :rate])
            opened (get-in tree [id :open])
            tree (update-in tree [id :visited] (fnil inc 0))
            could-open? (and (not opened) (< 0 rate))
            o-tunnels (if could-open? tunnels (remove #(= % parent) tunnels))
            o-tree (if could-open? (assoc-in tree [id :open] true) tree)
            o-time-left (if could-open? (dec time-left) time-left)
            o-value (if could-open? (+ value (* rate o-time-left)) value)
            possible-actions (for [open? (if could-open? [true false] [false])
                                   tunnel o-tunnels]
                               [open? tunnel])]
        (if (seq o-tunnels)
          (reduce (fn [paths [open? tunnel]]
                    (let [opened-child? (get-in tree [tunnel :open])
                          rate-child (get-in tree [tunnel :rate])
                          open-child? (and (not opened-child?) (< rate rate-child))
                          tree (if open? o-tree tree)
                          time-left (if open? o-time-left time-left)
                          value (if open? o-value value)]
                      (if (and open? open-child?) paths
                          (walk-tree tree tunnel (dec time-left) id paths value))))
                  paths possible-actions)
          (max paths value)))))

(defn parse-line
  [line]
  (let [[_ valve rate tunnels :as match] (re-find #"Valve (\w\w) has flow rate=(\d+); tunnels? leads? to valves? ((?:\w\w\,? ?)+)" line)
        tunnels (mapv keyword (str/split tunnels #", "))]
    [(keyword valve) {:rate (util/str->int rate) :tunnels tunnels}]))

(defn part1
  [filename]
  (let [tree (->> filename util/load-input
                  (map parse-line)
                  (into {}))]
    (walk-tree tree :AA 30 nil 0 0)))

(defn part2
  [filename]
  (let [tree (->> filename util/load-input
                  (map parse-line)
                  (into {}))]
    (elefanttimarssi tree :AA :AA nil nil 26 0 0)))
