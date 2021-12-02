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
      {(keyword opp) (Integer/parseInt value)})))

;; part 1
(loop [input (parse input)
       pos {:x 0 :y 0}]
  (let [head (first input)
        tail (rest input)]
    (if (nil? head) pos
     (case (apply key head)
           :down (recur tail (update pos :y #(+ % (:down head))))
           :up (recur tail (update pos :y #(- % (:up head))))
           :forward (recur tail (update pos :x #(+ % (:forward head))))))))

;; part 2
(loop [input (parse input)
       pos {:x 0 :y 0 :aim 0}]
  (let [head (first input)
        tail (rest input)]
    (if (nil? head) pos
        (case (apply key head)
          :down (recur tail (update pos :aim #(+ % (:down head))))
          :up (recur tail (update pos :aim #(- % (:up head))))
          :forward (recur tail 
                    (as-> pos $
                      (update $ :x #(+ % (:forward head)))
                      (update $ :y #(+ % (* (:aim $) (:forward head))))))))))
