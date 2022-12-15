(defn parse-instruction [line]
  (let [pair (clojure.string/split line #" ")
        dir (first pair)
        step (Integer/parseInt (second pair))]
    [dir step]))

(defn sign [integer]
  (-> integer float Math/signum int))

(defn min [a b c d]
  (Math/min (Math/min (Math/min a b) c) d))

(defn max [a b c d]
  (Math/max (Math/max (Math/max a b) c) d))

(defn render [instruction head1 tail1 head2 tail2]
  (let [min-x (min (get head1 :x) (get tail1 :x) (get head2 :x) (get tail2 :x))
        max-x (max (get head1 :x) (get tail1 :x) (get head2 :x) (get tail2 :x))
        min-y (min (get head1 :y) (get tail1 :y) (get head2 :y) (get tail2 :y))
        max-y (max (get head1 :y) (get tail1 :y) (get head2 :y) (get tail2 :y))
        grid-y-range (range (dec min-y) (inc max-y))
        grid-x-range (range (dec min-x) (inc max-x))]
    ;(println min-x)
    ;(println max-x)
    ;(println min-y)
    ;(println max-y)
    (println "")
    (println (str "dir: ") (first instruction) ", step: " (second instruction))
    (println (str "head: " head1))
    (println (str "tail: " tail1))

    (doseq [y grid-y-range]

      (doseq [x grid-x-range]

        (cond
          (and (= x (get head1 :x)) (= y (get head1 :y)))
          (print "H")

          (and (= x (get tail1 :x)) (= y (get tail1 :y)))
          (print "T")

          (and (= x (get head2 :x)) (= y (get head2 :y)))
          (print "1")

          (and (= x (get tail2 :x)) (= y (get tail2 :y)))
          (print "2")

          :else
          (print "-")))
      (println ""))


    (println "===")))

; part 1
(with-open [reader (clojure.java.io/reader "C:\\Git\\AdventOfCode2022\\Data\\Day 9.txt")]
  (let [lines (doall (line-seq reader))
        instructions (map parse-instruction lines)]
    (loop [instruction-head (take 10 instructions)
           head-pos {:x 0 :y 0}
           tail-pos {:x 0 :y 0}
           visited #{{:x 0 :y 0}}]
      (if (some? instruction-head)
        (let [instruction (first instruction-head)
              ;_ (println instruction)
              dir (first instruction)
              step (second instruction)
              new-head-pos (case dir
                             "U" (assoc head-pos :y (- (get head-pos :y) step))
                             "D" (assoc head-pos :y (+ (get head-pos :y) step))
                             "L" (assoc head-pos :x (- (get head-pos :x) step))
                             "R" (assoc head-pos :x (+ (get head-pos :x) step)))
              tail-to-head-dir {:x (- (get new-head-pos :x) (get tail-pos :x))
                                :y (- (get new-head-pos :y) (get tail-pos :y))}
              tail-to-head-dir-sign {:x (sign (get tail-to-head-dir :x))  :y (sign (get tail-to-head-dir :y))}
              is-x-move (not= 0 (get tail-to-head-dir :x))
              is-y-move (not= 0 (get tail-to-head-dir :y))
              is-2-steps-away (or (>= (abs (get tail-to-head-dir :x)) 2)
                                  (>= (abs (get tail-to-head-dir :y)) 2))
            ;step-1 (dec step)
            ;tail-dir-scaled { :x (* (get tail-to-head-dir :x) step-1) :y (* (get tail-to-head-dir :y) step-1) }
              dir-int (case dir
                        "U" -1
                        "D" 1
                        "L" -1
                        "R" 1)
              new-tail-pos (if is-2-steps-away
                             (cond
                             ; diagonal
                               (and is-x-move is-y-move)
                               {:x (+ (get tail-pos :x) (get tail-to-head-dir-sign :x)) :y (+ (get tail-pos :y) (get tail-to-head-dir-sign :y))}

                             ; horizontal
                               is-x-move
                               {:x (- (get tail-pos :x) dir-int) :y (get tail-pos :y)}

                             ; vertical
                               is-y-move
                               {:x (get head-pos :x) :y (- (get head-pos :y) dir-int)})
                             tail-pos)
              visited-new (conj visited new-tail-pos)]
          (render instruction new-head-pos new-tail-pos head-pos tail-pos)
          (recur (next instruction-head)
                 new-head-pos
                 new-tail-pos
                 visited-new))

          ; return result
        (count visited)))))