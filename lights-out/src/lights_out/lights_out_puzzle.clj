(ns lights-out.lights-out-puzzle)

(def t-vec
  [
   [true true true true true]
   [true true true true true]
   [true true true true true]
   [true true true true true]
   [true true true true true]
  ]
)

(def simple-start-vec
  [
   [false false false false false]
   [false false true true false]
   [false true true true false]
   [true false true false false]
   [false true false false false]
  ]
)

(defn light-heuristic
  [state-map]
  (count (filter not (flatten state-map))))

(defn get-adjacent [[x y] min max]
  (filterv
    (fn [[x y]] (and (<= min x max)
                      (<= min y max)))
    [[x y] [(+ x 1) y] [(- x 1) y] [x (+ y 1)] [x(- y 1)]]
  )
)
