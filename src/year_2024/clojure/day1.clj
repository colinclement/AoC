(ns year-2024.clojure.day1
  (:require [clojure.string :as str])
  (:require [clojure.repl :refer :all]) ; for doc, apropos, etc
  )

(defn get_location_ids [p]
  (let [lines (str/split-lines
               (slurp p :encoding "UTF-8"))]
    (mapv #(apply vector (mapv Integer/parseInt (re-seq #"\d+" %))) lines)))

(def ids (get_location_ids "inputs/1.txt"))

(comment
  (count ids)
  (subvec ids 0 5)
  [mapv ]
  :rcf)

(defn summed-sorted-difference [idlist]
  (let [l (apply vector (sort (mapv #(get % 0) idlist)))
        r (apply vector (sort (mapv #(get % 1) idlist)))] 
    (apply + (mapv #(abs (- %2 %1)) r l))))



(defn similarity-score [l r]
  (let [freq (frequencies r)]
    ;(swap! counts #(update % :a (constantly 1)))
    (apply + (mapv #(* (get freq % 0) %) l))
  )
)


;; part one
;; compute the sorted order absolute difference of the two lists of numbers
(summed-sorted-difference ids)


;; part two
(similarity-score (mapv #(get % 0) ids) (mapv #(get % 1) ids))