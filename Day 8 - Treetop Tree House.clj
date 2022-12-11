; couldn't figure out matrix rotations in clojure, so took the
; code from here in the end: https://www.reddit.com/r/adventofcode/comments/zfpnka/comment/izg9a7j
; learned you can use (vector list list) to zip, 
; ->> chaining with values array updated along the way, alse reverse to flip grid
; and fn's can be written (fn [] []) so directly return a vector

(defn inspect [tline vline] (
  ->> (map vector tline vline)
    (reduce
      (fn [[highest line] [tree visibility]] [
        (max tree highest)
        (->> (> tree highest) (or visibility) (conj line))
      ])
      [-1 []]
    )
    second
))
 
(defn find-visible [dir tline] (
  ->> tline
    (map-indexed vector)
    (reduce
      (fn [line [ix step]] (
        cond
          (zero? ix) (conj line (assoc step dir 0))
          (== 1 ix) (conj line (assoc step dir 1))
          :else (
            loop [prev (dec ix)] (
              if (or (>= (get-in line [prev "val"]) (step "val")) (zero? prev))
                (conj line (assoc step dir (- ix prev)))
                (recur (- prev (get-in line [prev dir])))
            ))))
      []
    )
))

; part 1
(with-open [rdr (clojure.java.io/reader "D:\\Development\\AdventOfCode2022\\Data\\Day 8.txt")] (
  let [matrix (->> rdr line-seq (map #(for [digit %] (-> digit str (Integer/parseInt)))))] (
    ->> matrix
      ((fn [matrix] [matrix (map #(map neg? %1) matrix)]))
                                                                                            
      ((fn [[tmatrix vmatrix]] [tmatrix (map inspect tmatrix vmatrix)])) 
                                                                                            
      ((fn [[tmatrix vmatrix]] [(map reverse tmatrix) (map reverse vmatrix)]))
      ((fn [[tmatrix vmatrix]] [tmatrix (map inspect tmatrix vmatrix)]))
                                                                                            
      ((fn [[tmatrix vmatrix]] [(apply map vector tmatrix) (apply map vector vmatrix)]))
      ((fn [[tmatrix vmatrix]] [tmatrix (map inspect tmatrix vmatrix)]))
                                                                                            
      ((fn [[tmatrix vmatrix]] [(map reverse tmatrix) (map reverse vmatrix)]))
      ((fn [[tmatrix vmatrix]] (map inspect tmatrix vmatrix)))
                                                                                            
      flatten
      (filter true?)
      count
    )))

; part 2
(with-open [rdr (clojure.java.io/reader "D:\\Development\\AdventOfCode2022\\Data\\Day 8.txt")] (
  let [matrix (->> rdr line-seq (map #(for [digit %] (-> digit str (Integer/parseInt)))))] (
    ->> matrix
      (#(map (fn [line] (vec (map (fn [tree] {"val" tree}) line))) %1))
      ((fn [matrix] (map (partial find-visible "left") matrix)))
      (#(map reverse %))
      ((fn [matrix] (map (partial find-visible "right") matrix)))
      (#(apply map vector %))
      ((fn [matrix] (map (partial find-visible "up") matrix)))
      (#(map reverse %))
      ((fn [matrix] (map (partial find-visible "down") matrix)))
      flatten
      ((fn [trees] (map #(apply * (-> % (dissoc "val") vals)) trees)))
      (apply max)
    )
))