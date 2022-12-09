(defn update-dir-sizes [initial-state filesize]
  (loop [path (get initial-state :dir-current)
         state initial-state]
    (if (not (empty? path))
      (->>
       (let [current-size (get-in state [:dir-sizes path] 0)
             new-size (+ current-size filesize)]
         (assoc-in state [:dir-sizes path] new-size))
       ; next iteration, process dir above
       (recur (pop path)))

      ; else: on process all dirs in chain, return new state
      state)))

(defn build-directories-map [reader]
  (loop [lines (line-seq reader)
         state {:dir-current ["/"]
                :dir-sizes {}}]
    (if (some? lines)
      (->>
       (let [line (first lines)]
         (cond
            ; ignore
           (clojure.string/starts-with? line "$ ls")
           state

            ; ignore
           (clojure.string/starts-with? line "dir")
           state

            ; move to root
           (clojure.string/starts-with? line "$ cd /")
           (assoc state :dir-current ["/"])

            ; move up one dir
           (clojure.string/starts-with? line "$ cd ..")
           (let [path (get state :dir-current)
                 prev-path (pop path)]
             (assoc state :dir-current prev-path))

            ; move down one dir
           (clojure.string/starts-with? line "$ cd")
           (let [datums (clojure.string/split line #" ")
                 new-dir (last datums)
                 path (get state :dir-current)
                 next-path (conj path new-dir)]
             (assoc state :dir-current next-path))

            ; file size
           :else
           (let [pair (clojure.string/split line #" ")
                 filesize (Integer/parseInt (first pair))]
             (update-dir-sizes state filesize))))

       (recur (next lines)))

      ; else: return result
      state)))

; part 1
(with-open [reader (clojure.java.io/reader "D:\\Development\\AdventOfCode2022\\Data\\Day 7.txt")]
  (let [state (build-directories-map reader)
        sizes (vals (get state :dir-sizes))
        sizes-under-100k (filter #(< % 100000) sizes)
        total (reduce + sizes-under-100k)]
    total))

(defn java-binary-search [v value]
  (java.util.Collections/binarySearch v value compare))

; part 2
(with-open [reader (clojure.java.io/reader "D:\\Development\\AdventOfCode2022\\Data\\Day 7.txt")]
  (let [state (build-directories-map reader)
        used-space (get-in state [:dir-sizes ["/"]])
        unused-space (- 70000000 used-space)
        required-space (- 30000000 unused-space)
        sizes (vals (get state :dir-sizes))
        sizes-sorted (sort sizes)
        bs-index (java-binary-search sizes-sorted required-space)
        index (-> bs-index (+ 1) (* -1))
        closest-size (nth sizes-sorted index)]
    closest-size))