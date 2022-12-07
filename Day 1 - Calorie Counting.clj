(defn parseInt [str]
  (if (empty? str) 0 (Integer/parseInt str)))

; part 1 - 68292
(with-open [reader (clojure.java.io/reader "D:\\Development\\AdventOfCode2022\\Data\\Day 1.txt")]
  (let [lines (doall (line-seq reader))
        groups (partition-by empty? lines)
        sums (map #(reduce + (map parseInt %)) groups)
        max (reduce max sums)]
    max))

; part 2
(with-open [reader (clojure.java.io/reader "D:\\Development\\AdventOfCode2022\\Data\\Day 1.txt")]
  (let [lines (doall (line-seq reader))
        groups (partition-by empty? lines)
        sums (map #(reduce + (map parseInt %)) groups)
        sorted (reverse (sort-by max sums))
        top3 (take 3 sorted)
        total (reduce + top3)]
    total))