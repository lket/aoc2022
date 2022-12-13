(ns aoc2022.util
  "Utils for AOC2022"
  (:require [clojure.string :as str]
            [clojure.pprint :as p]
            [clojure.java.io :as io]))

(defn str->int [str] (Integer/parseInt str))

(defn load-input
  [filename]
  (-> filename
      io/resource
      slurp
      str/split-lines))

(defn dbg
  "Debug print value"
  [value]
  (p/pprint value)
  value)

(defn dbgv
  "Debug print type"
  [value]
  (println (type value))
  value)

(defmacro dbgf
  "Debug print form"
  [form]
  (p/pprint form)
  form)

(defmacro varmista
  ([f nimi] `(~f ~(str nimi)))
  ([f nimi pitäisi]
   `(let [tulos# (~f ~(str nimi))]
      (assert (= tulos# ~pitäisi))
      :ok!)))
