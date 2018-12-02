(ns adventofcode.2017.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (mapv #(Integer/parseInt (str %)) (slurp (io/resource "2017/input1.txt"))))

(defn solution1 [input]
  (+ (if (= (first input)
            (last input))
       (first input))
    (reduce + (loop [i input
                      matches []]
                 (if (seq i)
                   (if (= (first i)
                          (second i))
                     (recur (rest i) (conj matches (first i)))
                     (recur (rest i) matches))
                   matches)))))


(defn solution2 [input]
  (let [shift (/ (count input) 2)]
    (reduce + (remove nil? (map-indexed (fn [i elem]
                                          (when (= elem (get input (mod (+ shift i)
                                                                        (count input))))
                                            elem))
                                        input)))))