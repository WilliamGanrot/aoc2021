(ns aoc2021.d3
  (:require [clojure.string :as str]))

(defn binary->base10 [b]
  (Long/parseLong b 2))

(def input
  (->> "resources/d3.txt"
       slurp
       str/split-lines
       (map (fn [s] (mapv #(Long/parseLong (str %)) s)))))

(defn- get-digit-counts [arr]
  (->> arr
       (map frequencies)))

(def test-input (str/split-lines "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"))

(defn freq-digits-at-index [input index]
  (->> input 
       (map #(nth % index)) 
       frequencies
       (sort-by key)
       (map (fn [[_ v]] v))))


(defn freq-digits-at-indexes [input]
  (->> (first input)
     count
     range
     (map #(freq-digits-at-index test-input %))))


(defn gamma-binary [input]
  (reduce (fn [acc [zero one]]
            (str acc (if (> zero one) 1 0)))
          ""
          input))

(defn epsilon-binary [input]
  (reduce (fn [acc [zeros ones]] 
            (str acc (if (< ones zeros) 1 0)))
          ""
          input))

(defn oxygen-binary [input]
  (reduce (fn [acc [zeros ones]]
            (str acc (if (>= ones zeros) 1 0)))
          ""
          input))

(defn co2-binary [input]
  (reduce (fn [acc [zeros ones]]
            (str acc (if (<= zeros ones) 0 1)))
          ""
          input))

(freq-digits-at-indexes test-input)
(oxygen-binary (freq-digits-at-indexes test-input))
(co2-binary (freq-digits-at-indexes test-input))

;;p1
(let [i (freq-digits-at-indexes test-input)]
  (->> [(epsilon-binary i) (gamma-binary i)]
       (map #(binary->base10 %))
       (reduce * 1)))




(defn count-digits [binary] 
  (->> binary
       (frequencies)
       vals))

(->> test-input
     (map #(count-digits %)))


(defn- get-digit-counts [arr]
  (->> arr
       (apply map vector)
       (map frequencies)
       (mapv #(sort-by val %))))

(defn- mult-bin [arrs]
  (->> arrs
       (map #(Long/parseLong (apply str %) 2))
       (apply *)))


;; part 1
(->> test-input
     get-digit-counts
     )

(defn- get-digit-with [default select-fn kvs]
  (if (apply == (map val kvs))
    default
    (key (select-fn kvs))))

(defn- find-row [default select-fn]
  (loop [idx 0
         candidates input]
    (if (== 1 (count candidates))
      (first candidates)
      (let [counts (get-digit-counts candidates)
            digit (get-digit-with default select-fn (counts idx))
            new-candidates (filter #(== digit (% idx)) candidates)]
        (recur (inc idx) new-candidates)))))

;; part 2
(->> [[1 second] [0 first]]
     (map #(apply find-row %))
     mult-bin)