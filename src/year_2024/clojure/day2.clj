(ns year-2024.clojure.day2
  (:require [clojure.core :refer :all]
            [clojure.test :as test]
            [clojure.string :as str]); for doc, apropos, etc
  )

(defonce data-string (slurp "inputs/2.txt"))

(defn parse-data [inp]
  (->> (str/split-lines inp)
       (map split)))

(def data-list (parse-data data-string))

(def test-data "7 6 4 2 1
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
       (count)))

(test/deftest test-part-1
  (test/is (= 2 (count-safe test-data))))

(count-safe data-string)

(comment
  (test/run-tests))

;; part 2

;; one strategy: work with diffs; removing an element is the same
;; as summing an adjacent pairs of diffs before doing the same
;; monitonic and one-to-three logic
;; 2 1 2 4 5 3 -> -1 1 2 1 -2

(defn diffs-less-one [inp]
  (let [d (diff inp)
        l (dec (count inp))]
    (for [i (range 0 (inc l))]
      (cond
        (= i 0) (rest d)
        (= i l) (drop-last d)
        :else (filter some? (for [j (range 0 l)]
                              (cond (= (dec i) j) nil
                                    (= j i) (+ (nth d j) (nth d (dec j)))
                                    :else (nth d j))))))))

(defn any-safe-less-one [inp]
  (some true? (map safe (diffs-less-one inp))))

(defn count-safe-part-two [inp]
  (->> (str/split-lines inp)
       (map split)
       (map any-safe-less-one)
       (filter true?)
       (count)
       ))


(test/deftest test-part-2
  (test/is (= 4 (count-safe-part-two test-data))))

(count-safe-part-two data-string)