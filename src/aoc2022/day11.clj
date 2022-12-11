(ns aoc2022.day11
  (:require [clojure.string :as str]
            [aoc2022.util :as util]))

(defn infix
  [a op b]
  (op a b))

(defn parse-monkey
  [lines]
  (let [monkey (str/join \newline lines)
        edn (-> monkey
                (str/replace #"Monkey (\d):" "{ :id $1")
                (str/replace #"Starting items: (.+)\n" ":items [$1]")
                (str/replace #"Operation: new = (.+)\n" ":op (fn [old] (infix $1))")
                (str/replace #"Test: divisible by (\d+)\n" ":test $1")
                (str/replace #"If true: throw to monkey (\d)\n" ":true $1")
                (str/replace #"If false: throw to monkey (\d)" ":false $1 :handled 0 }"))
        data (clojure.edn/read-string edn)]
    (update data :op eval)))

(defn eval-monkey
  [monkey]
  (let [item (first (:items monkey))
        new-monkey (-> monkey
                       (assoc :items (vec (rest (:items monkey))))
                       (update :handled inc))
        op (:op monkey)
        test (:test monkey)
        op-result (op item)
        ;; part1:
        ;; worry int (/ op-result 3))
        ;; part2:
        worry (mod (bigint op-result) (:common monkey))
        test-result (= (mod worry test) 0)]
    [new-monkey worry (if test-result (:true monkey) (:false monkey))]))

(defn id-map [list]
  (reduce (fn [result item] (assoc result (:id item) item)) {} list))

(defn monkey-business
  [monkeys id]
  (if (seq (:items (get monkeys id)))
    (let [[new-monkey item to] (eval-monkey (get monkeys id))
          monkeys (-> monkeys
                      (update-in [to :items] #(conj % item))
                      (assoc (:id new-monkey) new-monkey))]
      (recur monkeys (:id new-monkey)))
    monkeys))

(defn keep-away-round
  [n monkeys]
  (reduce monkey-business monkeys (flatten (repeat n (map first monkeys)))))

(defn solve
  [filename]
  (let [monkeys (->> filename util/load-input
                     (partition-all 7)
                     (map parse-monkey))
        common (apply * (map #(:test %) monkeys))
        monkeys (map #(assoc %1 :common common) monkeys)]
    (->> monkeys
         id-map
         (keep-away-round 10000)
         (map second)
         (map :handled)
         (sort)
         (take-last 2)
         (apply *))))

(assert (= (solve "day11_input") 11309046332))
