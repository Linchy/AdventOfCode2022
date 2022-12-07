(defn split-line [line]
  (let [pairs (clojure.string/split line #",")
        vectors (map #(let [split (clojure.string/split % #"-")
                            lhs (-> split first Integer/parseInt)
                            rhs (-> split last Integer/parseInt)] [lhs rhs]) pairs)]
    vectors))

(defn contains [lhs rhs]
  (let [containsLeft (>= (first rhs) (first lhs))
        containsRight (<= (last rhs) (last lhs))
        containsBoth (and containsLeft containsRight)]
    containsBoth))

(defn contains-either [[lhs rhs]]
  (or (contains lhs rhs)
      (contains rhs lhs)))

; part 1
(with-open [reader (clojure.java.io/reader "D:\\Development\\AdventOfCode2022\\Data\\Day 4.txt")]
  (let [lines (doall (line-seq reader))
        pairs (map split-line lines)
        containers (filter contains-either pairs)
        count (count containers)]
    count))

(defn to-range-sets [[lhs rhs]]
  (let [lhsRange (range (first lhs) (+ (last lhs) 1))
        rhsRange (range (first rhs) (+ (last rhs) 1))
        lhsSet (set lhsRange)
        rhsSet (set rhsRange)]
    [lhsSet rhsSet]))

; part 2
(with-open [reader (clojure.java.io/reader "D:\\Development\\AdventOfCode2022\\Data\\Day 4.txt")]
  (let [lines (doall (line-seq reader))
        pairs (map split-line lines)
        sets (map to-range-sets pairs)
        intersections (map #(clojure.set/intersection (first %) (last %)) sets)
        intersections-with-value (filter #(> (count %) 0) intersections)
        count (count intersections-with-value)]
    count))