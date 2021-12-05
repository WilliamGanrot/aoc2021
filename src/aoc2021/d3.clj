(ns aoc2021.d3
  (:require [clojure.string :as str]))

(defn binary->base10 [b]
  (Long/parseLong b 2))

(def input
  (->> "resources/d3.txt"
       slurp
       str/split-lines))

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
            (str acc (if (> one zero) 1 0)))
          ""
          input))

(defn epsilon-binary [input]
  (reduce (fn [acc [zeros ones]] 
            (str acc (if (< ones zeros) 1 0)))
          ""
          input))

(defn rate [input pred default]
  (loop [candidates input
         index 0]
    (if (= (count candidates) 1)
      (first candidates)
      (let [[zeros ones] (freq-digits-at-index candidates index)
            req-num (cond
                      (= ones zeros) default
                      (pred ones zeros) 1
                      :else 0)
            filterd-candidats (->> candidates
                                   (filter (fn [v] (= (str (nth v index)) (str req-num)))))]
        (recur filterd-candidats (inc index))))))

;; p1
(let [i (freq-digits-at-indexes test-input)]
  (->> [(epsilon-binary   i) (gamma-binary i)]
       (map #(binary->base10 %))
       (reduce * 1)))

;; p2
(->> [(rate input > 1) (rate input < 0)]
     (map #(binary->base10 %))
     (reduce * 1))