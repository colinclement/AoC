(ns year-2024.clojure.day5
  (:require
   [clojure.string :as str]
   [clojure.test :as test]))

(defonce input-string (slurp "inputs/5.txt"))

(def test-string "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
")

; nested map {"page", {"page required after", 1 "page required after", 1...}}
(defn get-rules [inp]
  (reduce
   #(assoc %1 (get %2 1) (assoc (%1 (get %2 1) {}) (get %2 2) 1))
   {}
   (re-seq #"(\d+)\|(\d+)\n" inp)))

(defn get-updates [inp]
  (->> (re-seq #"((?:d+)*(:?\d+,)+(?:\d+)*)" inp)
       (map #(nth % 0))
       (map #(str/split % #","))))

(defn is-ordered [upd rules]
  (= upd (sort #(contains? (rules %1) %2) upd)))


(defn part-one [inp]
  (let [rules (get-rules inp)
        updates (get-updates inp)]
    (->> (filter #(is-ordered % rules) updates)
         (map #(get % (quot (count %) 2)))
         (map read-string)
         (apply +))))

(part-one test-string)

(part-one input-string)