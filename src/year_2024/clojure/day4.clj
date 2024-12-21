(ns year-2024.clojure.day4
  (:require [clojure.core :refer :all]
            [clojure.string :as str]
            [clojure.test :as test]))

(def input-data (slurp "inputs/4.txt"))

(def test-string
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

;; strategy: generate all the sequences of horizontal,
;; vertical, and diagonal cuts of the sequence, and double
;; with reversal, then use regex to count number of XMAS in each
;; instead of doing something complicated with coordinates for
;; the diagonal I will just offset each row with spaces each
;; way and re-use the vertical extraction code
;; maybe easy way to do diagonal is to 'rotate' the sequence

(defn count-xmas [inp]
  (count (re-seq #"XMAS" inp)))

(defn add-reverse-all [inp]
  (concat inp (map str/reverse inp)))

(str/split-lines test-string)

(defn vertical-subsequences [s]
  (let [lines (str/split-lines s)
        W (count (peek (str/split-lines s)))
        H (count lines)
        chars (str/split (str/replace s #"\n" "") #"")]
    (for [x (range 0 W)]
      (apply str (for [y (range 0 H)]
                   (get chars (+ (* y W) x)))))))

; test if vertical of vertical is horizontal
(=
 (str/split-lines test-string)
 (vertical-subsequences
  (str/join "\n" (vertical-subsequences test-string))))


(defn shift-all [s right?]
  (let [lines (str/split-lines s)]
    (str/join "\n"
              (for [i (range 0 (count lines))]
                (apply str
                       (apply str
                              (if right?
                                (repeat i " ")
                                (repeat (- (count lines) i) " ")))
                       (get lines i)
                       (apply str
                              (if right?
                                (repeat (- (count lines) i) " ")
                                (repeat i " "))))))))

; (shift-all "XXXXXX\nXXXXXX\n...") =>
"
XXXXXX......
.XXXXXX.....
..XXXXXX....
...XXXXXX...
....XXXXXX..
.....XXXXXX.
......XXXXXX 
 "

(defn part-one [inp]
  (->> (concat
        ; all horizontal
        (add-reverse-all (str/split-lines inp))
        ; all vertical
        (add-reverse-all (vertical-subsequences inp))
        ; all diagonal
        (add-reverse-all (vertical-subsequences (shift-all inp true)))
        (add-reverse-all (vertical-subsequences (shift-all inp false))))
       (map count-xmas)
       (apply +)))

(part-one test-string)

(test/deftest test-part-1
  (test/is (= 18 (part-one test-string))))

(part-one input-data)

;; part 2

(defn get-coord [lines x y]
  (nth (nth lines y) x))

(defn is-x-mas [grid x y]
  (let [nw (get-coord grid (- x 1) (- y 1))
        ne (get-coord grid (+ x 1) (- y 1))
        se (get-coord grid (+ x 1) (+ y 1))
        sw (get-coord grid (- x 1) (+ y 1))
        s (str nw ne se sw)]
    (cond
      (= s "MMSS") 1
      (= s "SMMS") 1
      (= s "MSSM") 1
      (= s "SSMM") 1
      :else 0)))

(defn count-cross-mas [inp]
  (let [lines (str/split-lines inp)]
    (->> (for [x (range 1 (dec (count (peek lines))))]
           (for [y (range 1 (dec (count lines)))]
             (if (= (get-coord lines x y) \A) (is-x-mas lines x y) 0)))
         (map #(apply + %))
         (apply +))))

(count-cross-mas input-data)

(test/deftest part-two
  (test/is (= 9 (count-cross-mas test-string))))

(comment
  (test/run-tests))