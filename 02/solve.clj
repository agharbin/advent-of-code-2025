(ns advent.2025.2
  (:require
    [clojure.repl :as repl]
    [clojure.string :as s]
    [clojure.set :as set]
    [util :as u]))

;; Input

(defn parse-line [i]
  (let [[_ s e] (re-matches #"(\d+)-(\d+)" i)]
    [(parse-long s) (parse-long e)]))

(defn parse-input [i]
  (->> i
      s/trim
      (#(s/split % #","))
      (map parse-line)))

(def input (->> (slurp "input.dat") parse-input))

;; Part 1

(defn calculate-range-sum [[s e]]
  (loop [xs (range s (inc e))
         sum 0]
    (if (seq xs)
      (let [n (first xs)
            strn (str n)
            len (count strn)
            half-len (/ len 2)]
        (if (and (even? len) (= (subs strn 0 half-len) (subs strn half-len len)))
          (recur (rest xs) (+ sum n))
          (recur (rest xs) sum)))
      sum)))

(defn solve [xs]
  (->> (map calculate-range-sum xs) (apply +)))

(solve input)

;; Part 2

(defn is-invalid? [n]
  (let [strn (str n)
        len (count strn)]
    (loop [xs (range 1 len)]
      (if (seq xs)
        (if (and (zero? (mod len (first xs))) (apply = (partition (first xs) strn)))
          true
          (recur (rest xs)))
        false))))

(defn solve-2 [xs]
  (->> xs
    (mapcat #(range (first %) (inc (second %))))
    (filter is-invalid?)
    (apply +)))

(solve-2 input)
