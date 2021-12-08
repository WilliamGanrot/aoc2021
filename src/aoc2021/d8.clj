(ns aoc2021.d8
  (:require [clojure.string :as str]
            [clojure.data :as data]))

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
                  (map (fn [[input output]] {:input (remove empty (str/split input #" ")) :output (filter not-empty (str/split output #" "))}) $)))


(def input (as-> (str/split-lines (slurp "resources/d8.txt")) $
             (map #(str/split % #"\|") $)
             (map (fn [[input output]] {:input (str/split input #" ") :output (str/split output #" ")}) $)))

(def length-pattern {:0 6
                     :1 2
                     :2 5
                     :3 5
                     :4 4
                     :5 5
                     :6 6
                     :7 3
                     :8 7
                     :9 6})


(defn seg-difference [super sub]
  (str/join (->> (seq super)
       (filter #(not (some #{%} (seq sub)))))))


(defn match-segment-length [segment]
  (reduce (fn [acc, digit]
            (cond
              (= (count digit) (:1 length-pattern 0)) (assoc acc :1 (inc (:1 acc 0)))
              (= (count digit) (:4 length-pattern 0)) (assoc acc :4 (inc (:4 acc 0)))
              (= (count digit) (:7 length-pattern 0)) (assoc acc :7 (inc (:7 acc 0)))
              (= (count digit) (:8 length-pattern 0)) (assoc acc :8 (inc (:8 acc 0)))
              :else acc))
          {} segment))


;; part 1
(->> input
     (map #(match-segment-length (:output %)))
     (filter not-empty)
     (map #(vals (seq %)))
     flatten
     (reduce +))



(defn find-pattern [segments]
  (loop [remaining-keys [:1 :4 :7 :8 :2 :3 :5 :9 :0 :6]
         pattern {}
         segments segments]
    (let [key (first remaining-keys)]
      (if (= nil key)
        pattern
        (let [found-segment (case key
                              :1 (->> segments (filter #(= (count %) 2)) first)
                              :4 (->> segments (filter #(= (count %) 4)) first)
                              :7 (->> segments (filter #(= (count %) 3)) first)
                              :8 (->> segments (filter #(= (count %) 7)) first)
                              :3 (->> segments
                                      (filter #(= (count %) 5))
                                      (filter #(= 0 (count (seg-difference (:1 pattern) %))))
                                      first)

                              :2 (->> segments
                                      (filter #(= (count %) 5))
                                      (filter #(= 3 (count (seg-difference % (:4 pattern)))))
                                      first)

                              :5 (->> segments (filter #(= (count %) 5)) first)
                              :9 (->> segments
                                      (filter #(= 6 (count %)))
                                      (filter #(= 0 (count (seg-difference (:3 pattern) %))))
                                      first)
                              :6 (->> segments
                                      (filter #(= 6 (count %)))
                                      (filter #(= 1 (count (seg-difference (:1 pattern) %))))
                                      first)
                              :0 (->> segments
                                      (filter #(= 6 (count %)))
                                      (filter #(= 0 (count (seg-difference (:1 pattern) %)))))]
          (recur
           (rest remaining-keys)
           (assoc pattern key found-segment)
           (remove #{found-segment} segments)))))))


(defn segment-matches-pattern [segment pattern]
  (= (sort segment) (sort pattern)))

(defn match-pattern [pattern segments]
  (Long/parseLong (reduce (fn [acc, segment]
                            (cond
                              (segment-matches-pattern segment (:0 pattern)) (str acc "0")
                              (segment-matches-pattern segment (:1 pattern)) (str acc "1")
                              (segment-matches-pattern segment (:2 pattern)) (str acc "2")
                              (segment-matches-pattern segment (:3 pattern)) (str acc "3")
                              (segment-matches-pattern segment (:4 pattern)) (str acc "4")
                              (segment-matches-pattern segment (:5 pattern)) (str acc "5")
                              (segment-matches-pattern segment (:6 pattern)) (str acc "6")
                              (segment-matches-pattern segment (:7 pattern)) (str acc "7")
                              (segment-matches-pattern segment (:8 pattern)) (str acc "8")
                              (segment-matches-pattern segment (:9 pattern)) (str acc "9")))
                          "" segments)))

;; part2 
(->> test-input
     (map #(assoc % :pattern (find-pattern (:input %))))
     (map #(match-pattern (:pattern %) (:output %)))
     (reduce +))
