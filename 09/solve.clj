(ns advent.2025.9
  (:require
    [clojure.string :as s]
    [clojure.set :as set]))

;; Input

(defn parse-line [i]
  (mapv parse-long (s/split i #",")))

(defn parse-input [i]
  (->> i
       s/split-lines
       (mapv parse-line)))

(def input (->> (slurp "sample.dat") parse-input))

;; Part 1

(defn area [[x1 y1] [x2 y2]]
  (* (inc (abs (- x1 x2)))
     (inc (abs (- y1 y2)))))

(defn solve [pairs]
  (->> (for [x pairs y pairs :while (not= x y)]
         (area x y))
       (apply max)))

(solve input)

;; Part 2

; Approach is to find all unique x and y values appearing in the red tiles
; and partition the space into a grid of cells created by hypothetical lines
; at each x and y value. Then we determine which cells are 'green' by scanning
; left-to-right and tracking when we cross an edge. Finally, we sort all
; combinations of corners by descending area and find the first one for which
; all contained cells are green cells.

(defn collinear-with-vertical-edge? [indexed-vertical-edges vertical-line]
  (let [x-position (:x-position vertical-line)
        min-y (apply min (:y-extent vertical-line))
        max-y (apply max (:y-extent vertical-line))
        matching-edges (->> (indexed-vertical-edges {:x-position x-position})
                            (filter (fn [edge]
                                      (let [edge-min-y (apply min (:y-extent edge))
                                            edge-max-y (apply max (:y-extent edge))]
                                        (<= edge-min-y min-y max-y edge-max-y)))))]
    (not (empty? matching-edges))))

(defn compute-x-segments [i]
  (let [x-vals (sort (into #{} (map first i)))]
    (map vec (partition 2 1 x-vals))))

(defn compute-y-segments [i]
  (let [y-vals (sort (into #{} (map second i)))]
    (map vec (partition 2 1 y-vals))))

(defn compute-green-cells [i]
  (let [x-segments (compute-x-segments i)
        y-segments (compute-y-segments i)
        edge-segments (->> (partition 2 1 (conj i (first i))))
        vertical-edge-segments (->> edge-segments
                                    (filter (fn [[[x1 y1] [x2 y2]]] (= x1 x2)))
                                    (map (fn [[[x1 y1] [x2 y2]]] {:x-position x1 :y-extent [y1 y2]})))
        horizontal-edge-segments (->> edge-segments
                                    (filter (fn [[[x1 y1] [x2 y2]]] (= y1 y2)))
                                    (map (fn [[[x1 y1] [x2 y2]]] {:y-position y1 :x-extent [x1 x2]})))
        indexed-vertical-edge-segments (set/index vertical-edge-segments [:x-position])
        indexed-horizontal-edge-segments (set/index horizontal-edge-segments [:y-position])]
    (loop [ys y-segments
           xs x-segments
           was-outside? true
           included-cells #{}]
      (if (seq ys)
        (if (seq xs)
          (let [x-segment (first xs)
                y-segment (first ys)
                left-x-position (first x-segment)
                left-cell-edge {:x-position left-x-position :y-extent y-segment}
                crossed-box-boundary? (collinear-with-vertical-edge?
                                        indexed-vertical-edge-segments
                                        left-cell-edge)]
            (recur ys
                   (rest xs)
                   (if crossed-box-boundary? (not was-outside?) was-outside?)
                   (if (or (and was-outside? crossed-box-boundary?)
                           (and (not was-outside?) (not crossed-box-boundary?)))
                     (conj included-cells [x-segment y-segment])
                     included-cells)))
          (recur (rest ys) x-segments true included-cells))
        included-cells))))

(defn contains-box? [[[ax1 ax2] [ay1 ay2]] [[bx1 bx2] [by1 by2]]]
  (and (<= ax1 bx1 bx2 ax2)
       (<= ay1 by1 by2 ay2)))

(defn corners-to-box [[x1 y1] [x2 y2]]
  (let [min-x (min x1 x2)
        max-x (max x1 x2)
        min-y (min y1 y2)
        max-y (max y1 y2)]
    [[min-x max-x] [min-y max-y]]))

(defn box-area [[[x1 x2] [y1 y2]]]
  (* (inc (- x2 x1)) (inc (- y2 y1))))

(defn solve-2 [i]
  (let [green-cells (compute-green-cells i)
        x-segments (compute-x-segments i)
        y-segments (compute-y-segments i)
        cells (for [x x-segments y y-segments] [x y])
        boxes (for [corner1 i corner2 i :while (not= corner1 corner2)]
                (corners-to-box corner1 corner2))
        boxes-by-area (reverse (sort (for [b boxes] [(box-area b) b])))
        ordered-boxes (map second boxes-by-area)]
    (loop [xs ordered-boxes]
      (if (seq xs)
        (let [box (first xs)
              contained-cells (into #{} (filter #(contains-box? box %) cells))
              contained-outside-cells (set/difference contained-cells green-cells)
              valid? (empty? contained-outside-cells)
              area (box-area box)]
          (if valid?
            area
            (recur (rest xs))))
        nil))))

(solve-2 input)
