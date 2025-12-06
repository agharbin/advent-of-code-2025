(ns advent.2025.5
  (:require
    [clojure.string :as s]
    [clojure.set :as set]))

;; Input

(defn parse-input [i]
  (let [[ranges-str ids-str] (s/split i #"\n\n")
        ranges (->> (re-seq #"(\d+)-(\d+)" ranges-str) (map rest) (mapv #(mapv parse-long %)))
        ids (->> (s/split-lines ids-str) (mapv parse-long))]
    [ranges ids]))

(def input (->> (slurp "input.dat") parse-input))

;; Part 1

(defn solve [[ranges ids]]
  (->> (for [range ranges id ids] [range id])
       (filter (fn [[[s e] id]] (<= s id e)))
       (map second)
       (into #{})
       count))

(solve input)

;; Part 2

(defn overlap? [[s1 e1] [s2 e2]]
  (or (<= s1 s2 e1)
      (<= s1 e2 e1)
      (<= s2 s1 e2)
      (<= s2 e1 e2)))

(defn merge-ranges [[s1 e1] [s2 e2]]
  [(min s1 s2) (max e1 e2)])

(defn solve-2 [range-set]
  (let [overlapping-range (->> (for [r1 range-set r2 range-set :while (not= r1 r2)] [r1 r2])
                               (filter (fn [[x y]] (overlap? x y)))
                               first)]
    (if overlapping-range
      (let [[r1 r2] overlapping-range
            merged (merge-ranges r1 r2)]
        (recur (-> range-set (set/difference #{r1 r2}) (set/union #{merged}))))
      (->> range-set
           (map (fn [[s e]] (inc (- e s))))
           (apply +)))))

(solve-2 (set (first input)))
