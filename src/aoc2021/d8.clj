(ns aoc2021.d8
  (:require [clojure.string :as str]))

(def test-input (as-> (str/split-lines "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce") $
                  (map #(str/split % #"\|") $)
                  (map (fn [[input output]] {:input (str/split input #" ") :output (str/split output #" ")}) $)))


(def input (as-> (str/split-lines (slurp "resources/d8.txt")) $
             (map #(str/split % #"\|") $)
             (map (fn [[input output]] {:input (str/split input #" ") :output (str/split output #" ")}) $)))

(def pattern {:0 "abcefg"
               :1 "cf"
               :2 "acdeg"
               :3 "acdfg"
               :4 "bcdf"
               :5 "abdfg"
               :6 "abdefg"
               :7 "acf"
               :8 "abcdefg"
               :9 "abcdfg"})

(defn match-segment-length [segment]
  (reduce (fn [acc, digit]
            (println digit)
            (cond
              (= (count digit) (count (:1 pattern))) (assoc acc :1 (inc (:1 acc 0)))
              (= (count digit) (count (:4 pattern))) (assoc acc :4 (inc (:4 acc 0)))
              (= (count digit) (count (:7 pattern))) (assoc acc :7 (inc (:7 acc 0)))
              (= (count digit) (count (:8 pattern))) (assoc acc :8 (inc (:8 acc 0)))
              :else acc))
          {} segment))

(->> input
     (map #(match-segment-length (:output %)))
     (filter not-empty)
     (map #(vals (seq %)))
     flatten
     (reduce +)
     )
