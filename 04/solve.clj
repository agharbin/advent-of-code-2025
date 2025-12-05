(ns advent.2025.3
  (:require
    [clojure.string :as s]
    [util :as u]))

;; Input

(defn parse-line [i]
  (apply vector i))

(defn parse-input [i]
  (->> i
       s/split-lines
       (mapv parse-line)))

(def input (->> (slurp "input.dat") parse-input))

;; Part 1

(defn roll-with-fewer-than-four-neighbors? [[r c] grid]
  (let [neighbors (->> (u/eight-neighbors [r c])
                       (map #(get-in grid %))
                       (filter #(= % \@))
                       count)]
    (and (= \@ (get-in grid [r c])) (< neighbors 4))))

(defn solve [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (->> (for [r (range rows) c (range cols)] [r c])
         (filter #(roll-with-fewer-than-four-neighbors? % grid))
         count)))

(solve input)

;; Part 2

(defn rolls-with-fewer-than-four-neighbors [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (->> (for [r (range rows) c (range cols)] [r c])
         (filter #(roll-with-fewer-than-four-neighbors? % grid)))))

(defn remove-rolls [grid]
  (reduce (fn [m [r c]] (assoc-in m [r c] \.)) grid (rolls-with-fewer-than-four-neighbors grid)))

(defn remove-all-rolls [grid]
  (->> (iterate remove-rolls grid)
       (partition 2)
       (drop-while (fn [[x y]] (not= x y)))
       first
       first))

(defn count-rolls [grid]
  (->> (flatten grid) (filter #{\@}) count))

(defn solve-2 [grid]
  (let [initial-rolls (count-rolls grid)
        final-grid (remove-all-rolls grid)
        final-rolls (count-rolls final-grid)]
    (- initial-rolls final-rolls)))

(solve-2 input)
