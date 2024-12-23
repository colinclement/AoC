(ns year-2024.clojure.day6
  (:require
   [clojure.string :as str]
   [clojure.test :as test]))

(def test-string
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defonce input-string (slurp  "inputs/6.txt"))

(defn charray [inp]
  (map
   #(str/split % #"")
   (str/split-lines inp)))

(defn guard-pos [ch]
  (let [Ly (count ch)
        Lx (count (nth ch 0))
        guardset (set '("^" ">" "<" "v"))]
    (-> (for [y (range 0 Ly) x (range 0 Lx)]
          (if (contains? guardset (nth (nth ch y) x)) (list x y) nil))
        (#(filter some? %))
        (nth 0 nil))))

(defn oob [ch npos]
  (let [Ly (count ch) Lx (count (nth ch 0))
        x (nth npos 0) y (nth npos 1)]
    (or (< x 0) (= x Lx) (< y 0) (= y Ly))))

(defn grab [ch pos]
  (if (oob ch pos)
    "."
    (nth (nth ch (nth pos 1)) (nth pos 0))))

(defn up [pos] (list (nth pos 0) (dec (nth pos 1))))
(defn right [pos] (list (inc (nth pos 0)) (nth pos 1)))
(defn down [pos] (list (nth pos 0) (inc (nth pos 1))))
(defn left [pos] (list (dec (nth pos 0)) (nth pos 1)))

(defn move-pos [pos move]
  (case move
    :up (up pos)
    :right (right pos)
    :down (down pos)
    :left (left pos)
    :rotate pos))

(defn next-move [ch pos]
  (let [guard (grab ch pos)]
    (cond
      (= guard "^") (if (= (grab ch (up pos)) "#") :rotate :up)
      (= guard ">") (if (= (grab ch (right pos)) "#") :rotate :right)
      (= guard "v") (if (= (grab ch (down pos)) "#") :rotate :down)
      (= guard "<") (if (= (grab ch (left pos)) "#") :rotate :left))))

(defn make-rotate [ch pos]
  (let [guard (grab ch pos)
        rotate-guard {"^" ">" ">" "v" "v" "<" "<" "^"}]
    (for [[y row] (map-indexed list ch)]
      (if (not= y (nth pos 1))
        row
        (for [[x col] (map-indexed list row)]
          (if (not= x (nth pos 0))
            col
            (rotate-guard guard)))))))

(defn make-move [ch pos move]
  (let [npos (move-pos pos move)
        guard (grab ch pos)]

    (for [[y row] (map-indexed list ch)]
      (cond
        (and
         (not= y (nth pos 1))
         (not= y (nth npos 1))) row
        :else
        (for [[x col] (map-indexed list row)]
          (cond
            (and (= x (nth pos 0)) (= y (nth pos 1))) "X"
            (and (= x (nth npos 0)) (= y (nth npos 1))) guard
            :else col))))))

(defn guard-leave [ch pos]
  (for [[y row] (map-indexed list ch)]
    (if (not= y (nth pos 1))
      row
      (for [[x col] (map-indexed list row)]
        (if (not= x (nth pos 0))
          col
          "X")))))

(defn do-guard-path [chmap]

  (loop [ch chmap pos (guard-pos ch)]
    (let [move (next-move ch pos) nxpos (move-pos pos move)]
      (if (oob ch nxpos)
        (guard-leave ch pos)
        (if (= move :rotate)
          (recur (make-rotate ch pos) nxpos)
          (recur (make-move ch pos move) nxpos))))))

(defn part-one [chmap]
  (->> (do-guard-path chmap)
       (map (fn [row] (filter #(= % "X") row)))
       (map count)
       (apply +)))

(test/deftest test-part-one
  (test/is (= 41 (part-one (charray test-string))))
  (test/is (= 4982 (part-one (charray input-string))))
  )

(comment
  (test/run-tests))