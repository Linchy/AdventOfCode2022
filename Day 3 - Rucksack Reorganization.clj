(defn split-line [line]
  (let [length (.length line)
        lengthOver2 (/ length 2)
        lhs (set (subs line 0 lengthOver2))
        rhs (set (subs line lengthOver2 length))]
    [lhs rhs]))

(defn get-score [char]
  (if (Character/isLowerCase char)
    (+ (- (int char) (int \a)) 1)
    (+ (- (int char) (int \A)) 27)))

; part 1 - 8493
(with-open [reader (clojure.java.io/reader "C:\\Git\\AdventOfCode2022\\Data\\Day 3.txt")]
  (let [lines (doall (line-seq reader))
        pairs (map split-line lines)
        intersections (map #(clojure.set/intersection (first %) (last %)) pairs)
        items (flatten (map seq intersections))
        items-non-nil (filter some? items)
        scores (map get-score items-non-nil)
        total (reduce + scores)
        ]
    total))

; part 2
(with-open [reader (clojure.java.io/reader "C:\\Git\\AdventOfCode2022\\Data\\Day 3.txt")]
  (let [lines (doall (line-seq reader))
        sets (map set lines)
        groups (partition 3 sets)
        intersections (map #(clojure.set/intersection (nth % 0) (nth % 1) (nth % 2)) groups)
        items (flatten (map seq intersections))
        items-non-nil (filter some? items)
        scores (map get-score items-non-nil)
        total (reduce + scores)]
    total))