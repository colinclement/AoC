(ns year-2024.clojure.day2
  (:require [clojure.core :refer :all]
            [clojure.test :as test]
            [clojure.string :as str]); for doc, apropos, etc
  )

(defonce data-string (slurp "inputs/2.txt"))

(def test-data
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn split [s] (map read-string (str/split s #"\s+")))

(defn diff [x]
  (map - (rest x) x))

(defn monotonic [x]
  (or
   (every? neg? x)
   (every? pos? x)))

(defn one-to-three [x]
  (->> x
       (map abs)
       (map #(and (< 0 %) (< % 4)))
       (every? true?)))

(defn safe [x]
  (and (monotonic x) (one-to-three x)))

(defn count-safe [inp]
  (->> (str/split-lines inp)
       (map split)
       (map diff)
       (map safe)
       (filter true?)
       (count )))

(test/deftest test-part-1
  (test/is (= 2 (count-safe test-data))))

(count-safe data-string)

(comment
  (test/run-tests))