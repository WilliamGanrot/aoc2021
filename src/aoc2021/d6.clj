(ns aoc2021.d6
  (:require [clojure.string :as str]))

(def test-input (as-> "3,4,3,1,2" $
                  (str/split $ #",")
                  (map #(Integer/parseInt %) $)))

(def input (as-> "resources/d6.txt" $
             (slurp $)
             (str/split $ #",")
             (map #(Integer/parseInt %) $)))

(defn handle [fish]
  (cond
    (> fish 0) [(dec fish)]
    (= fish 0) [8 6]))

;;p1 - naive solution
(count (reduce (fn [fish _]
                 (->> fish (map #(handle %)) flatten)) test-input (range 1)))


(defn fish-freq-map [fish-input]
  (reduce (fn [acc [fish freq]]
            (assoc acc (keyword (str fish)) freq))
          {} (frequencies fish-input)))

(defn simulate-days [input days]
  (reduce (fn [fish-freq-map _]
            (hash-map
             :0 (:1 fish-freq-map 0)
             :1 (:2 fish-freq-map 0)
             :2 (:3 fish-freq-map 0)
             :3 (:4 fish-freq-map 0)
             :4 (:5 fish-freq-map 0)
             :5 (:6 fish-freq-map 0)
             :6 (+ (:7 fish-freq-map 0) (:0 fish-freq-map 0))
             :7 (:8 fish-freq-map 0)
             :8 (:0 fish-freq-map 0))) (fish-freq-map input) (range days)))

;, part 2
(->> (simulate-days input 256)
     vals
     (reduce +))


