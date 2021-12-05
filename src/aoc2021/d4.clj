(ns aoc2021.d4
  (:require [clojure.string :as str]))

(def input (slurp "resources/d4.txt"))


(def test-input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")


input
test-input

(defn parse [input]
  (let [input (as-> input $ (str/split $ #"\n\n"))
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

;; (defn winners [input]
;;   (loop [indexed-boards (map-indexed (fn [i v] [i v]) (first (rest input)))
;;          remaining (vec (first input))
;;          announced-numbers []
;;          total-winners []]
;;     (let [checked-cells (map (fn [[i v]] [i (get-checked-cells v announced-numbers)]) indexed-boards)
;;           board-results (map (fn [[i board]] {:bingo (bingo? board)
;;                                               :values (map #(Integer/parseInt (:cell %)) board)
;;                                               :board-index i
;;                                               :full-board (->> indexed-boards
;;                                                                (filter (fn [[li _]] (= li i)))
;;                                                                (map (fn [[_ l]] l))
;;                                                                (flatten)
;;                                                                (map #(Integer/parseInt %)))
;;                                               :last (parse-number (last announced-numbers))}) checked-cells)
;;           updated-total-winners (->> board-results
;;                        (filter #(= true (:bingo %)))
;;                        vec
;;                        (concat total-winners))
;;           remaining-boards (->> indexed-boards
;;                                 (filter (fn [[i _]] (not (some #{i} (map #(:board-index %) updated-total-winners))))))]
;;       (if (empty? remaining)
;;         updated-total-winners
;;         (recur
;;          remaining-boards
;;          (rest remaining)
;;          (conj announced-numbers (first remaining))
;;          updated-total-winners)))))

;; (defn p1 [input]
;;   (let [winner (first (winners input))]
;;   (* (- (reduce + (:full-board winner)) (reduce + (:values winner))) (:last winner))))



;; (defn p2 [input]
;;   (let [last-winner (last (winners input))]
;;   (* 
;;     (:last last-winner) 
;;     (- 
;;       (reduce + (:full-board last-winner)) 
;;       (reduce + (:values last-winner))))))









(defn winners2 [input]
  (loop [boards (map #(assoc {} :board %) (first (rest input)))
         remaining (vec (first input))
         announced-numbers []]
    (if (empty? remaining)
      (->> boards
           (filter #(= (:bingo %) true)))
      (let [prev-winners  (->> boards (filter #(= (:bingo %) true)))
            updated-boards (->> boards
                                (filter #(not= (:bingo %) true))
                                (map #(assoc % :bingo (bingo? (get-checked-cells (:board %) announced-numbers))))
                                (map #(assoc % :values announced-numbers))
                                )]
        (recur
         (concat prev-winners updated-boards)
         (rest remaining)
         (conj announced-numbers (first remaining)))))))


;;p1
(let [winner (first (winners2 (parse test-input)))
      board-sum (->> (:board winner)
                     flatten
                     (map #(Integer/parseInt %))
                     (reduce +))
      check-sum (->> (:values winner)
                     flatten
                     (map #(Integer/parseInt %))
                     (reduce +))]
  (* (- board-sum check-sum) (Integer/parseInt (last (:values winner)))))


(let [last-winner (last (winners2 (parse test-input)))
      board-sum (->> (:board last-winner)
                     flatten
                     (map #(Integer/parseInt %))
                     (reduce +))
      check-sum (->> (:values last-winner)
                     flatten
                     (map #(Integer/parseInt %))
                     (reduce +))]
  (*
   (Integer/parseInt (last (:values last-winner))) 
   (- board-sum check-sum))
  )

