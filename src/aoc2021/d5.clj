(ns aoc2021.d5
  (:require [clojure.string :as str]))

(def test-input (str/split-lines "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"))

(def input (str/split-lines (slurp "resources/d5.txt")))

(defn vertical? [instruction]
  (= (:ax instruction) (:bx instruction)))

(defn horizontal? [instruction]
  (= (:ay instruction) (:by instruction)))

(defn parse [input]
  (->> input
       (map #(let [[_ ax ay bx by] (re-find #"(\d),(\d)\s+->\s+(\d),(\d)" %)]
               {:ax (Long/parseLong ax) 
                :ay (Long/parseLong ay) 
                :bx (Long/parseLong bx) 
                :by (Long/parseLong by)}))))

(defn horizontal-path [instruction]
  (let [y (:ay instruction)
        [x1 x2] (sort [(:ax instruction) (:bx instruction)])
        xrange (range x1 (inc x2))]
    (->> xrange
         (map (fn [x] [x y])))))

(defn vertical-path [instruction]
  (let [x (:ax instruction)
        [y1 y2] (sort [(:ay instruction) (:by instruction)])
        yrange (range y1 (inc y2))]
    (->> yrange
         (map (fn [y] [x y])))))

(defn path [instruction]
  (case (:dir instruction)
    :horizontal (horizontal-path instruction)
    :vertical (vertical-path instruction)))

(defn instruction-info [input]
  (->> input
       parse
       (map #(cond
               (horizontal? %) (assoc % :dir :horizontal)
               (vertical? %) (assoc % :dir :vertical)
               :else (assoc % :dir :diagonal)))
       (filter #(not= :diagonal (:dir %)))
       (map #(assoc % :path (path %)))
       ))

(->> test-input
     instruction-info
     ;;
     (map #(:path %))
     ;;
     ;;
     (filter not-empty)
     
     ;;
     (mapcat identity)
     ;;count
     frequencies
     (filter (fn [[_ freq]] (> freq 1)))
     count
     )

