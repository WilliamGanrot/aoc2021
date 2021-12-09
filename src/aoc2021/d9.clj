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
                             :index [x y]
                             :adj {:top (get-cord x (dec y) height-map)
                                   :bottom (get-cord x (inc y) height-map)
                                   :left (get-cord (dec x) y height-map)
                                   :right (get-cord (inc x) y height-map)}}))))

(defn all-is-less-than [v list]
  (->> list
       (filter #(<= % v))
       (empty?)))


(defn lowpoints [input]
  (->> (cells-with-adjecents input)
       (filter (fn [map]
                 (->> (vals (:adj map))
                      (remove nil?)
                      (all-is-less-than (:cell map)))))))

;; part 1
(->> (lowpoints input)
     (map #(inc (:cell %)))
     (reduce +))


(defn point-in-point-map [point-map point]
  (->> point-map
       (filter #(= (:index %) (:index point)))
       first
       nil?
       not))

(defn remove-point-from-point-map [point-map point]
  (->> point-map
       (filter #(not= (:index %) (:index point)))))

(defn remove-range-from-point-map [point-map points]
  (reduce (fn [acc traversed-point]
            (remove-point-from-point-map acc traversed-point))
          point-map points))

;; part 2
(defn traverse [point point-map]
  (let [center (:cell point)
        [x y] (:index point)
        greater-adjs (->> (seq (:adj point))
                          (filter (fn [[_ v]] (and (not= nil v) (>= v center))))
                          (map (fn [[k _]] (case k
                                             :top [x (dec y)]
                                             :bottom [x (inc y)]
                                             :left [(dec x) y]
                                             :right [(inc x) y])))
                          (map (fn [xy] (->> point-map
                                             (filter #(= xy (:index %)))
                                             first)))
                          (filter #(point-in-point-map point-map %))
                          (filter #(not= 9 (:cell %))))]
    
    (cond
      (empty? greater-adjs) point
      :else (flatten [point (->> greater-adjs
                                 (map #(traverse % (remove-point-from-point-map point-map %))))]))))

(->> (lowpoints test-input)
     (reduce (fn [[acc point-map] point]
               (let [traversed (->> (traverse point point-map) distinct)
                     updated-point-map (remove-range-from-point-map point-map traversed)]
                 [(conj acc (count traversed)) (remove-point-from-point-map updated-point-map point)])) [[] (cells-with-adjecents test-input)])
     first
     sort
     reverse
     (take 3)
     (reduce *))