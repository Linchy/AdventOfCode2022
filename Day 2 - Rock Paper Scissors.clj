(def XRock "X")
(def YPaper "Y")
(def ZScissor "Z")

(def ARock "A")
(def BPaper "B")
(def CScissor "C")

(defn get-score [hand]
  (condp = hand
    XRock 1 ; rock
    YPaper 2 ; paper
    ZScissor 3)) ; scissors

(defn get-result [[opponent hand]]
  (let [lose 0
        draw 3
        win 6]
    (condp = opponent
      ; rock vs
      ARock (condp = hand XRock draw YPaper win  ZScissor lose)
      ; paper vs
      BPaper (condp = hand XRock lose YPaper draw ZScissor win)
      ; scissors vs
      CScissor (condp = hand XRock win  YPaper lose ZScissor draw))))

; part 1 
(with-open [reader (clojure.java.io/reader "D:\\Development\\AdventOfCode2022\\Data\\Day 2.txt")]
  (let [lines (doall (line-seq reader))
        pairs (map #(list (subs % 0 1) (subs % 2 3)) lines)
        scores (map #(+ (get-score (last %)) (get-result %)) pairs)
        total (reduce + scores)]
    total))

; part 2
(defn get-required-hand [[opponent result]]
  (case result
    ; need lose
    "X" (condp = opponent ARock ZScissor BPaper XRock CScissor YPaper)
    ; need draw
    "Y" (condp = opponent ARock XRock BPaper YPaper CScissor ZScissor)
    ; need win
    "Z" (condp = opponent ARock YPaper BPaper ZScissor CScissor XRock)))

(with-open [reader (clojure.java.io/reader "D:\\Development\\AdventOfCode2022\\Data\\Day 2.txt")]
  (let [lines (doall (line-seq reader))
        pairs (map #(list (subs % 0 1) (subs % 2 3)) lines)
        pairs-updated (map #(list (first %) (get-required-hand %)) pairs)
        scores (map #(+ (get-score (last %)) (get-result %)) pairs-updated)
        total (reduce + scores)]
    total))