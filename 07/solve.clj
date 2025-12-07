(ns advent.2025.7
  (:require
    [clojure.string :as s]
    [clojure.set :as set]
    [util :as u]))

;; Input

(defn parse-input [i]
  (->> i
       s/split-lines
       (mapv vec)))

(def input (->> (slurp "input.dat") parse-input))

;; Part 1

(defn solve [i]
  (let [first-line (first i)
        start-col (u/index-find first-line \S)]
    (loop [lines (rest i)
           beam-indexes #{start-col}
           splits 0]
      (if (seq lines)
        (let [line (first lines)
              splitter-indexes (u/index-find-all line \^)
              activated-splitters (set/intersection splitter-indexes beam-indexes)
              next-beam-indexes (-> beam-indexes
                                    (set/union (into #{} (map inc activated-splitters)))
                                    (set/union (into #{} (map dec activated-splitters)))
                                    (set/difference activated-splitters))]
          (recur (rest lines) next-beam-indexes (+ splits (count activated-splitters))))
        splits))))

(solve input)

;; Part 2

(defn solve-2 [i]
  (let [first-line (first i)
        line-length (count first-line)
        start-beam-index (u/index-find first-line \S)]
    (loop [lines (rest i)
           beam-counts (-> (repeat line-length 0) vec (assoc start-beam-index 1))]
      (if (seq lines)
        (let [line (first lines)
              splitter-indexes (u/index-find-all line \^)
              next-beam-counts (reduce
                                 (fn [beam-counts splitter-index]
                                   (-> beam-counts
                                       (update (dec splitter-index) #(+ % (beam-counts splitter-index)))
                                       (update (inc splitter-index) #(+ % (beam-counts splitter-index)))
                                       (assoc splitter-index 0)))
                                 beam-counts
                                 splitter-indexes)]
          (recur (rest lines) next-beam-counts))
        (apply + beam-counts)))))

(solve-2 input)
