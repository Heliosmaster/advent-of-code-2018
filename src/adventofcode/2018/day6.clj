(ns adventofcode.2018.day6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def day-input (map (comp (fn [row] [(Integer/parseInt (first row))
                                     (Integer/parseInt (last row))])
                          #(str/split % #", "))
                    (str/split-lines (slurp (io/resource "2018/input6.txt")))))


(def test-input [[1, 1] [1, 6] [8, 3] [3, 4] [5, 5] [8, 9]])


(defn distance [p1 p2]
  (+ (Math/abs (int (- (first p1)
                       (first p2))))
     (Math/abs (int (- (last p1)
                       (last p2))))))

(defn extreme-points [points]
  (let [xs (map first points)
        ys (map second points)
        xmin (apply min xs)
        xmax (apply max xs)
        ymin (apply min ys)
        ymax (apply max ys)]
    [[xmin ymin]
     [xmax ymax]]))

(defn grid-points [input]
  (let [xs (map first input)
        ys (map second input)]

    (for [x (range (apply min xs) (apply max xs))
          y (range (apply min ys) (apply max ys))]
      [x y])))

(defn closest-point [points p1]
  (let [distances (map (fn [p2]
                         {:point p2 :d (distance p1 p2)})
                       points)
        min-distance (:d (first (sort-by :d distances)))
        closest-points (filter #(= min-distance (:d %)) distances)]
    (when (= (count closest-points) 1)
      (:point (first closest-points)))))

(defn in-safe-region? [points p1]
  (< (reduce + (map (partial distance p1)
                    points))
     10000))


(defn solution1 [input]
  (->> (grid-points input)
       (pmap (partial closest-point input))
       (frequencies)
       (sort-by second)))



(defn solution2 [input]
  (->> (grid-points input)
       (filter (partial in-safe-region? input))
       (count)
       ))