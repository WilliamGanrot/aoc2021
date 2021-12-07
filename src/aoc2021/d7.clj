(ns aoc2021.d7
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(def test-input (as-> "16,1,2,0,4,2,7,1,2,14" $
                  (str/split $ #",")
                  (map #(Integer/parseInt %) $)))

(def input (as-> "resources/d7.txt" $
             (slurp $)
             (str/split $ #",")
             (map #(Integer/parseInt %) $)))

(defn total-distances-to [pos crabs step-inc]
  (->> crabs
       (map #(math/abs (- pos %)))
       (map #(+ % (* (/ (- (* % %) %) 2) step-inc)))
       (reduce +)))

(defn solve [input step-inc]
  (reduce (fn [min pos]
            (let [total-distance (total-distances-to pos input step-inc)]
              (if (< total-distance min)
                total-distance
                min)))
          ##Inf (range (reduce min input) (inc (reduce max input)))))


(solve input 1)