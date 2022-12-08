; thoughts: went the mutable/procedural route with this problem, via the use of atom,
;           swap!, compare-and-set! and dorun, dotimes etc. Turns out quite difficult
;           and messy to write clojure this way

(defn parse-instruction [instruction stacks]
  (let [[_ countStr sourceNumberStr destNumberStr] (re-matches #"move\s(\d+)\sfrom\s(\d+)\sto\s(\d+)" instruction)
        source (nth stacks (- (Integer/parseInt sourceNumberStr) 1))
        dest   (nth stacks (- (Integer/parseInt destNumberStr) 1))
        count (Integer/parseInt countStr)]
    [source dest count]))

(defn move-item-by-item [instruction stacks]
  (let [[source dest count] (parse-instruction instruction stacks)]
    ; perform each move of a single item from one stack to the other
    (dotimes [c count]
      (let [item (peek @source)]
        (swap! source pop)
        (swap! dest conj item)
        ;(println (str c " - " item "\r\n" @source "\r\n" @dest "\r\n"))
        ))))

(defn move-item-range [instruction stacks]
  (let [[source dest move-count] (parse-instruction instruction stacks)
        unmoved-count (- (count @source) move-count)
        ;_ (println (str "unmoved-count " unmoved-count " (count @source) " (count @source) " move-count " move-count "\r\n" @source "\r\n" @dest "\r\n"))
        items (subvec @source unmoved-count)
        dest-updated (vec (concat @dest items))]
    (swap! source subvec 0 unmoved-count)
    (compare-and-set! dest @dest dest-updated)
    ;(println (str items "\r\n" @source "\r\n" @dest "\r\n"))
    ))

(defn get-result [stacks]
  ;(println stacks)
  (map #(peek @%) stacks))

; part 1 - MQTPGLLDN
(with-open [reader (clojure.java.io/reader "D:\\Development\\AdventOfCode2022\\Data\\Day 5.txt")]
  (let [lines (doall (line-seq reader))
        instructions (drop 10 lines)
        stacks [(atom [\Z \J \N \W \P \S])         ; 1
                (atom [\G \S \T])                  ; 2
                (atom [\V \Q \R \L \H])            ; 3
                (atom [\V \S \T \D])               ; 4
                (atom [\Q \Z \T \D \B \M \J])      ; 5
                (atom [\M \W \T \J \D \C \Z \L])   ; 6
                (atom [\L \P \M \W \G \T \J])      ; 7
                (atom [\N \G \M \T \B \F \Q \H])   ; 8
                (atom [\R \D \G \C \P \B \Q \W])]  ; 9
        ; iterate instructions, use regex to pull the data
        _ (dorun (for [instruction instructions]
                   (move-item-by-item instruction stacks)))
        ; take top item of each stack, form a string
        top-chars (get-result stacks)
        result (apply str top-chars)]
    result))

; part 2 - LVZPSTTCZ
(with-open [reader (clojure.java.io/reader "D:\\Development\\AdventOfCode2022\\Data\\Day 5.txt")]
  (let [lines (doall (line-seq reader))
        instructions (drop 10 lines)
        stacks [(atom [\Z \J \N \W \P \S])         ; 1
                (atom [\G \S \T])                  ; 2
                (atom [\V \Q \R \L \H])            ; 3
                (atom [\V \S \T \D])               ; 4
                (atom [\Q \Z \T \D \B \M \J])      ; 5
                (atom [\M \W \T \J \D \C \Z \L])   ; 6
                (atom [\L \P \M \W \G \T \J])      ; 7
                (atom [\N \G \M \T \B \F \Q \H])   ; 8
                (atom [\R \D \G \C \P \B \Q \W])]  ; 9
        ; iterate instructions, use regex to pull the data
        _ (dorun (for [instruction instructions]
                   (move-item-range instruction stacks)))
        ; take top item of each stack, form a string
        top-chars (get-result stacks)
        result (apply str top-chars)]
    result))