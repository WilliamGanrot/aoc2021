(ns aoc2021.d2
  (:require [clojure.string :as str]))

(def test-input (str/split-lines "forward 5
down 5
forward 8
up 3
down 8
forward 2"))

(def input (str/split-lines (slurp "resources/d2.txt")))

(defn parse [input]
  (for [l input]
    (let [[opp value] (str/split l #" ")]
      [(keyword opp) (Integer/parseInt value)])))

;; part 1
(reduce (fn [map [key val]]
          (case key
            :down (update map :y #(+ % val))
            :up (update map :y #(- % val))
            :forward (update map :x #(+ % val))))
        {:x 0 :y 0}
        (parse test-input))

;; part 2
(reduce (fn [map [key val]]
          (case key
            :down (update map :aim #(+ % val))
            :up (update map :aim #(- % val))
            :forward (as-> map $
                       (update $ :x #(+ % val))
                       (update $ :y #(+ % (* (:aim $) val))))))
        {:aim 0 :y 0 :x 0}
        (parse test-input))

