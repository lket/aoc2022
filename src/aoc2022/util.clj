(ns aoc2022.util
  "Utils for AOC2022"
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as p])
  (:require [clojure.java.io :as io]))

(defn str->int [str] (Integer/parseInt str))

(defn load-input
  [filename]
  (-> filename
      io/resource
      slurp
      str/split-lines))

(defn dbg
  [value]
  (p/pprint value)
  value)

(defn dbgv
  [value]
  (println (type value))
  value)

(defmacro dbgf
  [form]
  (println form)
  form)
