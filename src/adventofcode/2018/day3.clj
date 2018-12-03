(ns adventofcode.2018.day3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (map (fn [row] (let [[id x y w h]
                                (map #(Integer/parseInt %) (drop 1 (re-matches #"#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)" row)))]
                            {:id id :x x :y y :w w :h h}))
                (str/split-lines (slurp (io/resource "2018/input3.txt")))))

(defn generate-tiles [{:keys [x y w h]}]
  (for [i (range w)
        j (range h)]
    [(+ x i) (+ y j)]))

(defn solution1 [input]
  (->> input
       (mapcat generate-tiles)
       (frequencies)
       (filter (fn [[_tile freq]] (> freq 1)))
       (count)))

(defn overlap? [t1 t2]
  (let [tiles1 (set (generate-tiles t1))
        tiles2 (set (generate-tiles t2))]
    (boolean (seq (clojure.set/intersection tiles1 tiles2)))))

(defn solution2 [input]
  (filter (fn [t1]
            (prn [:checking (:id t1)])
            (every? (fn [t2]
                      (or (= (:id t1) (:id t2))
                          (not (overlap? t1 t2))))
                    input))
          input))