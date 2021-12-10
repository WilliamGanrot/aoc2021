(ns aoc2021.d10
  (:require [clojure.string :as str]))

(def test-input (str/split-lines "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"))

(def input (str/split-lines (slurp "resources/d10.txt")))

(defn score1 [c] ({\) 3, \] 57, \} 1197, \> 25137} c))
(defn score2 [c] ({\) 1, \] 2, \} 3, \> 4} c))
(defn closing [c] ({\< \>, \{ \}, \[ \], \( \)} c))

(defn matching-pair [a b]
  (or
   (and (= a \() (= b \)))
   (and (= a \[) (= b \]))
   (and (= a \{) (= b \}))
   (and (= a \<) (= b \>))))

(defn parse [input]
  (loop [[head & tail] (seq input)
         stack []]
    (let [closing (closing head)]
      (cond
        (matching-pair (last stack) head) (recur tail (pop stack)) ;; if head matches first in stack
        (some? closing) (recur tail (conj stack head))             ;; if head is opening param
        (and (nil? head) (empty? stack)) {:result :valid}
        (some? head)                     {:result {:corrupt head}}
        :else                            {:result {:incomplete stack}}))))


;; part 1
(->> input
     (map #(parse %))
     (filter #(some? (get-in % [:result :corrupt])))
     (map #(get-in % [:result :corrupt]))
     (map #(score1 %))
     (reduce +))

;; part 2
(->> input
     (map #(parse %))
     (filter #(some? (get-in % [:result :incomplete])))
     (map #(get-in % [:result :incomplete]))
     (map #(reverse %))
     (map (fn [rev-stack] (->> rev-stack
                               (map #(closing %))
                               (map #(score2 %))
                               (reduce (fn [score-acc score]
                                         (+ (* score-acc 5) score)) 0))))
     (sort)
     (#(nth % (quot (count %) 2))))
