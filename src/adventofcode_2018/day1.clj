(ns adventofcode-2018.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (map #(Integer/parseInt %) (str/split-lines (slurp (io/resource "input1.txt")))))

(defn solution1 [input]
  (reduce + input))

(defn solution2 [input]
  (loop [i input
         result 0
         seen #{}]
    (let [new-result (+ (first i) result)]
      (if (contains? seen new-result)
        new-result
        (if (seq (rest i))
          (recur (rest i) new-result (conj seen new-result))
          (recur input new-result (conj seen new-result)))))))