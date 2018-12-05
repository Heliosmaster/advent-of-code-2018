(ns adventofcode.2018.day5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (lazy-seq (slurp (io/resource "2018/input5.txt"))))


(def test-input (lazy-seq "dabAcCaCBAcCcaDA"))

(defn odd-pairs [input]
  (partition-all 2 input))

(defn even-pairs [input]
  (conj (partition-all 2 (rest input))
        (take 1 input)))

(defn solution1 [input]
  (loop [i input
         round-without-matches 0
         odd-pairs? false]
    (if (<= round-without-matches 2)
      (let [pairs ((if odd-pairs? odd-pairs even-pairs) i)
            new-pairs (remove (fn [s] (and (= 2 (count s))
                                           (not= (first s) (last s))
                                           (= (str/lower-case (first s))
                                              (str/lower-case (last s)))))
                              pairs)
            new-i (apply concat new-pairs)]
        (if (= (count new-i)
               (count i))
          (recur new-i (inc round-without-matches) (not odd-pairs?))
          (recur new-i 0 (not odd-pairs?))))
      (count (apply str i)))))

(def letters (map char (concat (range 65 91))))

(defn solution2 [input]
  (for [l letters]
    (let [new-input (remove (fn [c] (or (= (str/lower-case l) (str/lower-case c)))) input)]
      (when (< (count new-input)
               (count input))
        (prn "Removing " l " - " (solution1 new-input))))))