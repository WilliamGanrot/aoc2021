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

(defn parse-number [s]
  (cond
    (nil? s) nil
    (re-find #"\d+" s) (Integer/parseInt s)
    :else nil))

(defn winners [input]
  (loop [indexed-boards (map-indexed (fn [i v] [i v]) (first (rest input)))
         remaining (vec (first input))
         announced-numbers []
         total-winners []]
    (let [checked-cells (map (fn [[i v]] [i (get-checked-cells v announced-numbers)]) indexed-boards)
          board-results (map (fn [[i board]] {:bingo (bingo? board)
                                              :values (map #(Integer/parseInt (:cell %)) board)
                                              :board-index i
                                              :full-board (->> indexed-boards
                                                               (filter (fn [[li _]] (= li i)))
                                                               (map (fn [[_ l]] l))
                                                               (flatten)
                                                               (map #(Integer/parseInt %)))
                                              :last (parse-number (last announced-numbers))}) checked-cells)
          updated-total-winners (->> board-results
                       (filter #(= true (:bingo %)))
                       vec
                       (concat total-winners))
          remaining-boards (->> indexed-boards
                                (filter (fn [[i _]] (not (some #{i} (map #(:board-index %) updated-total-winners))))))]
      (if (empty? remaining)
        updated-total-winners
        (recur
         remaining-boards
         (rest remaining)
         (conj announced-numbers (first remaining))
         updated-total-winners)))))

(defn p1 [input]
  (let [winner (first (winners input))]
  (* (- (reduce + (:full-board winner)) (reduce + (:values winner))) (:last winner))))

(defn p2 [input]
  (let [last-winner (last (winners input))]
  (* 
    (:last last-winner) 
    (- 
      (reduce + (:full-board last-winner)) 
      (reduce + (:values last-winner))))))


