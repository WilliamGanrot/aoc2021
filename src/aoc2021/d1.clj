(ns aoc2021.d1
  (:require [clojure.string :as str]))

(def test-input (map #(Integer/parseInt %) (str/split-lines "199
200
208
210
200
207
240
269
260
263")))

(def input (map #(Integer/parseInt %) (str/split-lines (slurp "resources/d1.txt"))))

(defn is-sorted? [l]
  (apply < l))

(defn p1 [input]
  (->> input
       (partition 2 1)
       (filter is-sorted?)
       (count)))

(p1 input)

(defn p2 [input]
  (->> input
       (partition 3 1)
       (map #(reduce + %))
       (partition 2 1)
       (filter is-sorted?)
       (count)))

(p2 input)