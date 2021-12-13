(ns aoc2021.d11
  (:require [clojure.string :as str]))

(def test-input (str/split-lines "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"))

(def input (str/split-lines (slurp "resources/d11.txt")))


(defn parse [input]
  (->> input
       (map #(seq %))
       (map (fn [row] (->> row (map #(Character/digit % 10)))))
       (map-indexed (fn [y row]
                      (map-indexed (fn [x charge] {:index [x y]
                                                   :charge charge
                                                   :adj {:top [x (dec y)]
                                                         :top-right [(inc x) (dec y)]
                                                         :top-left [(dec x) (dec y)]
                                                         :left [(dec x) y]
                                                         :right [(inc x) y]
                                                         :bottom [x (inc y)]
                                                         :bottom-right [(inc x) (inc y)]
                                                         :bottom-left [(dec x) (inc y)]}}) row)))
       (mapcat identity)))

(defn get-squid-at-cord [squid-map cord] (->> squid-map
                                              (filter #(= (:index %) cord))
                                              first))

(defn inc-cord [squids cord]
  (->> squids
       (map #(if (= cord (:index %)) (update % :charge inc) %))))

(defn inc-adjs [squids cord]
  (->> (vals (:adj (get-squid-at-cord squids cord)))
       (reduce (fn [acc-squids adj-cord] (inc-cord acc-squids adj-cord)) squids)))

(defn set-flashed [squids cord]
  (->> squids
       (map (fn [squid] (if (= cord (:index squid)) 
                          (assoc squid :flashed true)
                          squid)))))

;; (defn flashing [squids squid-cord]
;;   (let [squid (get-squid-at-cord squids squid-cord)
;;         flashed? (= true (:flashed squid))]
;;     (cond
;;       (nil? squid) squids
;;       (and (> 9 (:charge squid)) (not flashed?)) (let [updated-squids (inc-adjs (set-flashed squids squid-cord) squid-cord)]
;;                                                    (reduce (fn [squids cord]
;;                                                              (flashing squids cord)) updated-squids (vals (:adj squid))))
;;       :else squids)))


;; (defn step2 [squids]
;;   (let [updated-squids (->> squids (map #(update % :charge inc)))]
;;     (->> squids
;;          (map #(:index %))
;;          (reduce (fn [squids cord] (flashing squids cord)) updated-squids)
;;          (map (fn [squid] (if (> (:charge squid) 9) (assoc squid :charge 0) squid))))))

(defn flashing [squids]
  (let [about-to-flash-cords (->> squids 
                            (filter #(and (> (:charge %) 9) (not= true (:flashed %))))
                            (map #(:index %)))]
    (if (empty? about-to-flash-cords)
      squids
      (let [x (->> about-to-flash-cords
                   (reduce (fn [acc-squids cord]
                             (set-flashed (inc-cord (inc-adjs acc-squids cord) cord) cord)) squids))]
        
        (flashing x)))))

(defn step [squids]
  (let [flashed (->> squids 
                   (map #(update % :charge inc))
                   (#(flashing %)))
        flasing-count (->> flashed
                           (filter #(> (:charge % 0) 9))
                           count)]
    [flasing-count
     (->> flashed
          (map #(if (> (:charge %) 9) (assoc % :charge 0) %))
          (map #(dissoc % :flashed)))]))


(->> (range 100)
     (reduce (fn [[count squids] _]
               (let [[c s] (step squids)]
                 [(+ c count) s])) [0 (parse input)])
     first)

