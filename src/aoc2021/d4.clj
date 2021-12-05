(ns aoc2021.d4
  (:require [clojure.string :as str]))

(def input (slurp "resources/d4.txt"))

(defn parse [input]
  (let [input (as-> input $ (str/split $ #"\r\n\r\n"))
        numbers (as-> (first input) $
                  (str/split $ #","))
        boards (->> input
                    rest
                    (map #(str/split-lines %))
                    (map #(map (fn [r] (remove empty? (str/split r #"\s+"))) %)))]
    [numbers boards]))

(defn get-checked-cells
  "takes a board and all checkd value and return cordinats of all the checked value in the board"
  [board numbers]
  (remove nil? (mapcat identity
                       (map-indexed (fn [y row]
                                      (map-indexed (fn [x cell] (if (not= nil (some #{cell} numbers)) {:x x :y y :cell cell} nil)) row)) board))))

(defn bingo?
 "if more than four cordinats at same x or y is checked" 
  [checked]
  
  (let [xvalues (map (fn [v] (:x v)) checked)
        yvalues (map (fn [v] (:y v)) checked)
        ybingo (->> yvalues
                    frequencies
                    (filter (fn [[_ v]] (> v 4)))
                    (seq)
                    (some?))
        xbingo (->> xvalues
                    frequencies
                    (filter (fn [[_ v]] (> v 4)))
                    (seq)
                    (some?))]
    (not= nil (some #{true} (list xbingo ybingo)))))

(defn winners [input]
  (loop [boards (map #(assoc {} :board %) (first (rest input)))
         remaining (vec (first input))
         announced-numbers []]
    (if (empty? remaining)
      (->> boards 
           (filter #(= (:bingo %) true)))
      (let [prev-winners  (->> boards (filter #(= (:bingo %) true)))
            updated-boards (->> boards
                                (filter #(not= (:bingo %) true))
                                (map #(assoc % :bingo (->> announced-numbers
                                                           (get-checked-cells (:board %))
                                                           bingo?)))
                                (map (fn [board] (assoc board :values (->> announced-numbers
                                                                           (get-checked-cells (:board board))
                                                                           (map #(:cell %))))))
                                (map #(assoc % :last (last announced-numbers))))]
        (recur
         (concat prev-winners updated-boards)
         (rest remaining)
         (conj announced-numbers (first remaining)))))))

;; part 1
(let [winner (first (winners2 (parse input)))
      board-sum (->> (:board winner)
                     flatten
                     (map #(Integer/parseInt %))
                     (reduce +))
      check-sum (->> (:values winner)
                     flatten
                     (map #(Integer/parseInt %))
                     (reduce +))]
  (* (- board-sum check-sum) (Integer/parseInt (:last winner))))

;; part2
(let [last-winner (last (winners2 (parse input)))
      board-sum (->> (:board last-winner)
                     flatten
                     (map #(Integer/parseInt %))
                     (reduce +))
      check-sum (->> (:values last-winner)
                     flatten
                     (map #(Integer/parseInt %))
                     (reduce +))]
  (*
   (Integer/parseInt (:last last-winner)) 
   (- board-sum check-sum)))

