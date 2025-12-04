(ns advent.2025.3
  (:require
    [clojure.repl :as repl]
    [clojure.string :as s]
    [clojure.set :as set]
    [clojure.math.numeric-tower :as math]
    [util :as u]))

;; Input

(defn parse-line [i]
  (->> i
       seq
       (map str)
       (mapv parse-long)))

(defn parse-input [i]
  (->> i
       s/split-lines
       (map parse-line)))

(def input (->> (slurp "input.dat") parse-input))

;; Part 1

(defn highest-joltage [xs]
  (let [len (count xs)
        largest-first-digit (apply max (take (- len 1) xs))
        list-after-largest (->> xs (drop-while #(not= % largest-first-digit)) rest)
        largest-second-digit (apply max list-after-largest)]
    (+ largest-second-digit (* 10 largest-first-digit))))

(defn solve [i]
  (->> i
       (map highest-joltage)
       (apply +)))

(solve input)

;; Part 2

(defn select-highest-digits [xs n]
  (if (zero? n)
    (list)
    (let [len (count xs)
          candidate-list (take (-> len (- n) (+ 1)) xs)
          first-digit (apply max candidate-list)
          list-after-largest (->> xs (drop-while #(not= % first-digit)) rest)]
      (conj (select-highest-digits list-after-largest (dec n)) first-digit))))

(defn to-number [digits]
  (->> (u/zip (reverse digits) (range))
       (map (fn [[b n]] (* b (math/expt 10 n))))
       (apply +)))

(defn solve-2 [i]
  (->> i
       (map #(select-highest-digits % 12))
       (map to-number)
       (apply +)))

(solve-2 input)
