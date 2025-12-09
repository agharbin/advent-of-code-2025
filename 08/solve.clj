(ns advent.2025.8
  (:require
    [clojure.math :as math]
    [clojure.string :as s]))

;; Input

(defn parse-line [i]
  (mapv parse-long (re-seq #"\d+" i)))

(defn parse-input [i]
  (->> i
       s/split-lines
       (map parse-line)))

(def input (->> (slurp "input.dat") parse-input))

;; Part 1

(defn make-set [s i]
  (assoc s i {:parent i :size 1}))

(defn set-find [s i]
  (let [p (get-in s [i :parent])]
    (if (= i p)
      p
      (recur s p))))

(defn union [s [i j]]
  (let [ip (set-find s i)
        jp (set-find s j)
        ips (s ip)
        jps (s jp)]
    (if (= ip jp)
      s
      (if (<= (:size ips) (:size jps))
        (-> s
            (assoc-in [ip :parent] jp)
            (assoc-in [jp :size] (+ (:size jps) (:size ips))))
        (-> s
            (assoc-in [jp :parent] ip)
            (assoc-in [ip :size] (+ (:size jps) (:size ips))))))))

(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (math/sqrt
    (+ (math/pow (- x2 x1) 2)
       (math/pow (- y2 y1) 2)
       (math/pow (- z2 z1) 2))))

(defn distances [points]
  (for [p1 points p2 points :while (not= p1 p2)]
    [(distance p1 p2) p1 p2]))

(def unions 1000)
(def sum-size 3)

(defn solve [i]
  (let [sets (reduce make-set {} i)
        pairs-by-distance (->> (distances i) sort (take unions))
        nearest-pairs (map rest pairs-by-distance)]
    (->> (reduce union sets nearest-pairs)
         (filter (fn [[k v]] (= k (:parent v))))
         (map (fn [[k v]] (:size v)))
         sort
         (take-last sum-size)
         (apply *))))

(solve input)

;; Part 2

(defn solve-2 [i]
  (let [n (count i)
        pairs-by-distance (->> (distances i) sort)
        nearest-pairs (map rest pairs-by-distance)]
    (loop [sets (reduce make-set {} i)
           pairs nearest-pairs]
      (if (seq pairs)
        (let [pair (first pairs)
              merged-sets (union sets pair)
              size-of-merge (->> (first pair) (set-find merged-sets) merged-sets :size)]
          (if (= n size-of-merge)
            (apply * (map first pair))
            (recur merged-sets (rest pairs))))
        nil))))

(solve-2 input)
