  (defn queue
    ([] clojure.lang.PersistentQueue/EMPTY)
    ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn maintain-queue [queue max char]
  (let [queue2 (if (= (count queue) max) (pop queue) queue) ; dequeue
        queue3 (conj queue2 char)] ; enqueue
    queue3))

; part 1 - 1876
(with-open [r (clojure.java.io/input-stream "D:\\Development\\AdventOfCode2022\\Data\\Day 6.txt")]
  (loop [c (.read r)
         num 1
         last4 (queue)]
    (if (not= c -1)
      (let [last4-current (maintain-queue last4 4 (char c))
            distinct (set last4-current)
            has4-distinct (= (count distinct) 4)]
        (if has4-distinct
          num
          (recur (.read r)
                 (inc num)
                 last4-current))))))

; part 2 - 2202
(with-open [r (clojure.java.io/input-stream "D:\\Development\\AdventOfCode2022\\Data\\Day 6.txt")]
  (loop [c (.read r)
         num 1
         last14 (queue)]
    (if (not= c -1)
      (let [last14-current (maintain-queue last14 14 (char c))
            distinct (set last14-current)
            has14-distinct (= (count distinct) 14)]
        (if has14-distinct
          num
          (recur (.read r)
                 (inc num)
                 last14-current))))))