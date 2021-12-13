(ns aoc2021.d13
  (:require [clojure.string :as str]))

(slurp "resources/d13.txt")
(def test-input (let [[s1 s2] (str/split (slurp "resources/d13.txt") #"\r\n\r\n")]
{:cords (->> (str/split-lines s1)
          (map #(str/split % #","))
          (map (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)])))
 :folds (->> (str/split-lines s2)
          (map (fn [row] (let [[_ fold-dir v] (re-find #".+(x|y)=(\d+)" row)]
                           [(keyword fold-dir) (Integer/parseInt v)]))))}))


(defn fold-up [cord-list fold-y]
  (let [bottom-half-cords (->> cord-list (filter (fn [[_ y]] (> y fold-y))))
        top-half-cords (->> cord-list (filter (fn [[_ y]] (< y fold-y))))]
    (distinct (concat
               top-half-cords
               (->> bottom-half-cords
                    (map (fn [[x y]] [x (- (* 2 fold-y) y)])))))))

(defn fold-left [cord-list fold-x]
  (let [left-half-cords (->> cord-list (filter (fn [[x _]] (< x fold-x))))
        right-half-cords (->> cord-list (filter (fn [[x _]] (> x fold-x))))]
    (distinct (concat
               left-half-cords
               (->> right-half-cords
                    (map (fn [[x y]] [(- (* 2 fold-x) x) y])))))))


;; part 1
(count (let [cords (:cords test-input)
             [dir v] (first (:folds test-input))]
  (case dir
    :x (fold-left cords v)
    :y (fold-up cords v))))

;; part 2
(sort (reduce (fn [cords [fold-dir v]]
                (case fold-dir
                  :x (fold-left cords v)
                  :y (fold-up cords v))) (:cords test-input) (:folds test-input)))