(ns climbing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; part  a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn simple-path-count
  "How many ways are there to climb a mountain of size n?"
  [n]
  (Math/pow 2 (dec n)))

; The number of winning paths increases by a power of 2 for every row.

(simple-path-count 5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; part  b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn path-count-with-traps
  "How many ways are there to climb this 'mountain' with traps?
   'mountain' represents a mountain of size n = (count mountain),
   and (nth mountain i) is a String with exactly i+1 'O's or 'X's 
   and the rest spaces.
   "
  [mountain]
  (defn blocked? [node]
    (= "X" (str node)))

  (let [bottom-row (dec (count mountain))
        mount (map clojure.string/trim mountain)]
    (defn count-paths [row col]
      (cond (blocked? (nth (nth mount row) col)) 0
            (= row bottom-row) 1
            :else (+ (count-paths (inc row) col) ; lower-left node
                     (count-paths (inc row) (+ col 2))))) ; lower-right node
    (count-paths 0 0)))

(def mountain ["    0    ",
               "   0 0   ",
               "  0 X 0  ",
               " 0 0 X 0 "])

(path-count-with-traps mountain)

; In this case, we need to manually traverse the mountain to check for valid paths.
; Starting from the top, we recursively count the paths contained by the left and right
; child nodes. If a trap is encountered, we abort traversal of that path. If we reach
; the bottom row, that means we found a valid path.
