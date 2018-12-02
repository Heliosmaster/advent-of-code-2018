(ns adventofcode.2017.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (map (fn [row] (map #(Integer/parseInt %) (str/split row #"\t")))
                (str/split-lines (slurp (io/resource "2017/input2.txt")))
                ))

(defn solution1 [input]
  (reduce + (map (fn [row] (- (apply max row) (apply min row))) input)))


(defn solution2 [input]
  (reduce + (for [row input
                  x row
                  y row
                  :when (and (not= x y) (zero? (rem x y)))]
              (quot x y))))