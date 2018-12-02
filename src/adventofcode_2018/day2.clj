(ns adventofcode-2018.day2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (str/split-lines (slurp (io/resource "input2.txt"))))


(defn solution1 [input]
  (let [freqs (map (comp set distinct vals frequencies) input)]
    (* (count (filter #(contains? % 2) freqs))
       (count (filter #(contains? % 3) freqs)))))


(defn similar-boxes? [box1 box2]
  (<= (count (remove true? (map = box1 box2))) 1))

(defn solution2 [input]
  (map #(apply str %)
       (set (for [b1 input
                  b2 input
                  :when (and (not= b1 b2) (similar-boxes? b1 b2))]
              (remove nil? (last (clojure.data/diff (vec b1) (vec b2)))))))
  )