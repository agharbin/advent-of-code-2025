(ns advent.2025.1
  (:require
    [clojure.repl :as repl]
    [clojure.string :as s]
    [clojure.set :as set]
    [util :as u]))

;; Input

(defn parse-line [i]
  (let [[_ dir n] (re-matches #"(L|R)(\d+)" i)]
    [(keyword dir) (parse-long n)]))

(defn parse-input [i]
  (->> i
       s/split-lines
       (map parse-line)))

(def input (->> (slurp "input.dat") parse-input))

;; Part 1

(def start-pos 50)
(def dial-size 100)

(defn solve [i]
  (loop [xs i
         zeroes 0
         pos start-pos]
    (if (seq xs)
      (let [[d n] (first xs)
            next-pos (case d :L (- pos n) :R (+ pos n))
            next-pos (mod next-pos dial-size)]
        (recur (rest xs) (if (zero? next-pos) (inc zeroes) zeroes) next-pos))
      zeroes)))

(solve input)

;; Part 2

(defn solve-2 [i]
  (loop [xs i
         zeroes 0
         pos start-pos]
    (if (seq xs)
      (let [[d n] (first xs)
            next-pos (case d :L (dec pos) :R (inc pos))
            next-pos (mod next-pos dial-size)]
        (recur
          (if (> n 1) (conj (rest xs) [d (dec n)]) (rest xs))
          (if (zero? next-pos) (inc zeroes) zeroes)
          next-pos))
      zeroes)))

(solve-2 input)
