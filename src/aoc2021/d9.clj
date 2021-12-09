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

(defn lowpoints [input]
  (->> (cells-with-adjecents input)
       (filter (fn [map]
                 (->> (vals (:adj map))
                      (remove nil?)
                      (every? #(< (:cell map) %)))))))

;; part 1
(->> (lowpoints input)
     (map #(inc (:cell %)))
     (reduce +))

(defn remove-range-from-point-map [list range-to-remove]
  (reduce (fn [acc to-remove] (remove #{to-remove} acc)) list range-to-remove))

;; part 2
(defn traverse [point point-map]
  (let [[x y] (:index point)
        greater-adjs (->> (seq (:adj point))
                          (filter (fn [[_ v]] (and (not= nil v) (>= v (:cell point)))))
                          (map (fn [[k _]] (case k
                                             :top [x (dec y)]
                                             :bottom [x (inc y)]
                                             :left [(dec x) y]
                                             :right [(inc x) y])))
                          (map (fn [xy] (->> point-map
                                             (filter #(= xy (:index %)))
                                             first)))
                          (filter #(not= nil (some #{%} point-map)))
                          (filter #(not= 9 (:cell %))))]
    
    (cond
      (empty? greater-adjs) point
      :else (flatten [point (->> greater-adjs
                                 (map #(traverse % (remove #{%} point-map))))]))))

(defn traverse-points [point-map points]
  (->> points
       (reduce (fn [[acc point-map] point]
                 (let [traversed (->> (traverse point point-map) distinct)
                       updated-point-map (remove-range-from-point-map point-map traversed)]
                   [(conj acc (count traversed))
                    (remove #{point} updated-point-map)]))
               [[] point-map])
       first))

(let [point-map (cells-with-adjecents test-input)]
  (->> (lowpoints test-input)
       (traverse-points point-map)
       sort
       reverse
       (take 3)
       (reduce *)))
