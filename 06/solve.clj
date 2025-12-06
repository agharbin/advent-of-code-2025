(ns advent.2025
  (:require
    [clojure.string :as s]
    [util :as u]))

;; Input

(defn parse-input [i]
  (->> (s/split-lines i)
       (map s/trim)))

(def input (->> (slurp "input.dat") parse-input))

;; Part 1

(defn solve [[l1 l2 l3 l4 l5]]
  (->>
    [(map symbol (s/split l5 #"\s+"))
     (map parse-long (s/split l1 #"\s+"))
     (map parse-long (s/split l2 #"\s+"))
     (map parse-long (s/split l3 #"\s+"))
     (map parse-long (s/split l4 #"\s+"))]
    u/transpose
    (map seq)
    (map eval)
    (apply +)))

(solve input)

;; Part 2

(defn parse-input-2 [i]
  (->> (s/split-lines i)
       (mapv #(mapv str %))))

(def input-2 (->> (slurp "input.dat") parse-input-2))

(defn solve-2 [i]
  (let [max-line-length (apply max (map count i))
        padded-lines (for [line i]
                 (if (= max-line-length (count line))
                   line
                   (let [diff (- max-line-length (count line))]
                     (into line (repeat diff " ")))))
        transposed (reverse (u/transpose padded-lines))]
    (loop [lines transposed
           seen-numbers []
           accumulator 0]
      (if (seq lines)
        (let [line (first lines)
              digits (drop-last line)
              number (->> digits (apply str) s/trim parse-long)
              operator (last line)]
          (cond
            (every? #{" "} line) (recur (rest lines) [] accumulator)
            (= " " operator) (recur (rest lines) (conj seen-numbers number) accumulator)
            :else (recur
                    (rest lines)
                    []
                    (+ accumulator
                       (eval (conj (seq (conj seen-numbers number))
                                   (symbol operator)))))))
        accumulator))))

(solve-2 input-2)
