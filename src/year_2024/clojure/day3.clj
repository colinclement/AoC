(ns year-2024.clojure.day3
  (:require [clojure.core :refer :all]
            [clojure.string :as str]
            [clojure.test :as test]))

(def test-data "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn parse-list [s]
  (read-string (str "(" s  ")")))

(defn mult-acc-string [inp]
  (->> (map #(get % 1) (re-seq #"mul\((\d{1,3},\d{1,3})\)" inp))
       (map parse-list)
       (map #(apply * %))
       (apply +)))

(test/deftest test-part-1
  (test/is (= 161 (mult-acc-string test-data))))

(defonce data-string (slurp "inputs/3.txt"))

(mult-acc-string data-string)

;; part 2
(def test-data-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn lstrip-donts [s]
  (let [out (str/split s #"do\(\)")]
    (apply str (rest out))))

(defn strip-donts [slist]
  (let [dontsplit (str/split slist #"don't\(\)")]
    (apply str (for [i (range (count dontsplit))]
                 (if (= i 0) (get dontsplit 0)
                     (lstrip-donts (get dontsplit i)))))))

(defn part-two [inp]
  (->> (strip-donts inp)
       (mult-acc-string)))

(test/deftest test-part-2
  (test/is (= 48 (part-two test-data-2))))

(comment
  (test/run-tests))

(part-two data-string)