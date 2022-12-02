(ns aoc2022.util
  "Utils for AOC2022"
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))

(defn kissa
  [nimi]
  (format "Kissa nimeltä %s on söpö!" nimi))

(defn str->int [str] (Integer/parseInt str))

(defn load-input
  [filename]
  (-> filename
      io/resource
      slurp
      str/split-lines))
