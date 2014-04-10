(ns memoizer)

(defn bounded-memoize 
  "Return a bounded memoized version of fn 'f' 
   that caches the last 'k' computed values"
  [f k]
  (assert (and (fn? f) (integer? k)))
  (let [table (agent (array-map))]
    (fn [x]
      (let [memoized-result (get @table x)]
        (or memoized-result
            (let [result (f x)]
              (if-not (< (count @table) k)
                (send table dissoc (last (keys @table))))
              (send table assoc x result)
              result))))))

(defn memo-tester [x]
  (Thread/sleep 5000)
  (* x x))

(memo-tester 5)
(def memo-fn (bounded-memoize memo-tester 5))
(memo-fn 5)
(memo-fn 4)
(memo-fn 3)
(memo-fn 2)
(memo-fn 1)
(memo-fn 6) ; evicts 5 from memo table

; Usually I would use a hash-map to store the memo-table to get O(1) lookup.
; In this case, I want to maintain the insertion order for FIFO eviction
; so I used an array-map instead.

; I chose to wrap the table in an agent because agents are asynchronous,
; which gives us maximum throughput at the expense of possibly repeating some computations.
; I assumed the operations were uncoordinated and that order of exection wasn't important.
