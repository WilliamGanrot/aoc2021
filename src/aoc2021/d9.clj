(ns aoc2021.d9
  (:require [clojure.string :as str]))

(def test-input
  (->> (str/split-lines "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")
       (map #(seq %))
       (map (fn [row]
              (->> row
                   (map (fn [cell] (Character/digit cell 10))))))))

(def input
  (->> (str/split-lines (slurp "resources/d9.txt"))
       (map #(seq %))
       (map (fn [row]
              (->> row
                   (map (fn [cell] (Character/digit cell 10))))))))


(defn get-cord [x y map]
  (let [row (nth map y nil)
        cell (nth row x nil)]
    cell))

(defn index-map [map]
  (->> map
       (map-indexed (fn [y row]
                      (->> row
                           (map-indexed (fn [x cell] [cell [x y]])))))))


(defn cells-with-adjecents [height-map]
  (->> (index-map height-map)
       (mapcat identity)
       (map (fn [[v [x y]]] {:cell v
                             :top (get-cord x (dec y) height-map)
                             :bottom (get-cord x (inc y) height-map)
                             :left (get-cord (dec x) y height-map)
                             :right (get-cord (inc x) y height-map)}))))

(defn all-is-less-than [v list]
  (->> list
       (filter #(<= % v))
       (empty?)))


;; part 1
(->> (cells-with-adjecents input)
     (filter (fn [map]
               (->> (vals (select-keys map [:top :bottom :left :right]))
                    (remove nil?)
                    (all-is-less-than (:cell map)))))
     (map #(inc (:cell %)))
     (reduce +))
