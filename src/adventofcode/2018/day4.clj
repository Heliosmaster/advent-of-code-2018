(ns adventofcode.2018.day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (sort-by (juxt :year :month :day :hour :minutes)
                    (map (fn [row] (let [[year month day hour minutes message]
                                         (drop 1 (re-matches #"\[(\d+)-(\d+)-(\d+)\s(\d+):(\d+)\]\s(.*)" row))]
                                     {:year year :month month :day day :hour hour :minutes minutes :message message}))
                         (str/split-lines (slurp (io/resource "2018/input4.txt"))))))


(defn minutes-asleep [input]
  (loop [i input
         current-guard nil
         asleep-at nil
         result {}]
    (if (seq i)
      (let [m (first i)
            new-guard (re-matches #"Guard #(\d+) begins shift" (:message m))
            wakes-up (= "wakes up" (:message m))
            falls-asleep (= "falls asleep" (:message m))]
        (cond
          new-guard (recur (rest i) (last new-guard) nil result)
          falls-asleep (recur (rest i) current-guard (Integer/parseInt (:minutes m)) result)
          wakes-up (recur (rest i) current-guard nil (-> result (update current-guard (fnil concat []) (range asleep-at (Integer/parseInt (:minutes m))))))))
      result)))


(defn solution1 [input]
  (let [[guard-id minutes-sleeping] (->> (minutes-asleep input)
                                         (sort-by (fn [[_guard-id mins]] (count mins)))
                                         (last))
        chosen-minute (first (last (sort-by second (frequencies minutes-sleeping))))]
    {:guard-id      guard-id
     :chosen-minute chosen-minute
     :solution      (* (Integer/parseInt guard-id) chosen-minute)}))


(defn solution2 [input]
  (let [[guard-id [chosen-minute chosen-amount]] (->> (minutes-asleep input)
                                                      (map (fn [[guard-id mins]] [guard-id (last (sort-by second (frequencies mins)))]))
                                                      (into {})
                                                      (sort-by (fn [[guard-id [chosen-minute chosen-amount]]] chosen-amount))
                                                      (last))]
    {:guard-id      guard-id
     :chosen-minute chosen-minute
     :chosen-amount chosen-amount
     :solution      (* (Integer/parseInt guard-id) chosen-minute)}))