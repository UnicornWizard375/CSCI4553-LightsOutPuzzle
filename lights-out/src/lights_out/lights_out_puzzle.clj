(ns lights-out.lights-out-puzzle)

; (def t-vec
;   [
;    [true true true true true]
;    [true true true true true]
;    [true true true true true]
;    [true true true true true]
;    [true true true true true]
;   ]
; )

; (def simple-start-vec
;   [
;    [false false false false false]
;    [false false true true false]
;    [false true true true false]
;    [true false true false false]
;    [false true false false false]
;   ]
; )

; because I couldn't tell you in person: it turns out the vectors aren't mad anymore, 
; it's just that the third level of nesting is salmon colored on this linter.
; This brings the whole situation to a comedy.

(def simple-start-map
  {[0 0] false, [0 1] false, [0 2] false, [0 3] false, [0 4] false
   [1 0] false, [1 1] false, [1 2] true,  [1 3] true,  [1 4] false
   [2 0] false, [2 1] true,  [2 2] true,  [2 3] true,  [2 4] false
   [3 0] true,  [3 1] false, [3 2] true,  [3 3] false, [3 4] false
   [4 0] false, [4 1] true,  [4 2] false, [4 3] false, [4 4] false
   })

(def simple-start-problem
  {:grid simple-start-map, :min 0, :max 4})

(defn light-heuristic
  [state-map]
  (count (filter not (flatten state-map))))

(defn get-adjacent
  "Note that this also returns the node given as an arg.
This is because get-adjacent " 
  [[x y :as input] min max]
  (try
    (filterv
     (fn [[a b]] (and (<= min a max)
                      (<= min b max)))
     [
      [x y] 
      [(+ x 1) y] 
      [(- x 1) y] 
      [x (+ y 1)] 
      [x (- y 1)]
      ]
     )
    (catch Exception e (str "Caught exception: " (.getmessage e) \newline
                            "Get-adjacent was passed " input))
    )
    )

(defn get-adjacent-submap
  "Returns a submap of problem's light map containing only 
the light associated with node and the adjacent lights.
If refactoring occurs, this should be looked at."
  [node {grid :grid, min-val :min, max-val :max}]
  (select-keys (get-adjacent node min-val max-val) grid))


; (defn search
;   [{:keys [goal? heuristic]}
;    max-calls
;    ]
;  
;  
;   )


(defn toggle-light-by-list
  "This is a terrible, terrible hack.
So:  I want to iterate over a hashmap and return something that's almost the same,
but tweak a few elements.  The obvious way to do this is to use the map function
over the hash map.  The alternative way would be to go through the list, recursively 
conj-ing on either the original value or the tweaked value.  Problem: a map is actually
a sequence of MapEntrys, which is a weird hidden data type.  Hence the digging into clojure.lang.
This function takes a collection of the node coordinates to toggle and a grid of lights."
  [to-toggle grid]
  (map (fn [node] (if (contains? to-toggle (first node))
                    (clojure.lang.MapEntry/create (first node) (not (second node)))
                    node))
       grid))

(defn toggle-contains
  "Checks if val is in target.  If it is, it removes val from target.
If not, it adds val to target."
  [val target]
  (if (contains? target val)
    (disj target val)
    (conj target val)))

(defn node-heuristic [node problem]
  (let [adjacent-values (vals (get-adjacent-submap node problem))]
    (- (count (filter identity adjacent-values)) (count (filter not adjacent-values)))))

;assumes square grid
(defn greedy-search 
  "Takes a problem in the form of a square grid of lights, a minimum coordinate (normally 0),
and a maximum coordinate.  It then recursively toggles whichever node turns off the most lights
until all lights are off.  When they are, it prints a set of nodes.  Due to the properties of
the problem, any given node needs to be toggled a maximum of once, and the order in which the
nodes are toggled does not matter.  The set returned contains all lights which were toggled
an odd number of times during exploration, and as such toggling them in any order will provide
an efficient solution.  This is not guarenteed to be the best solution "
  ([problem]
   (greedy-search problem -1))
  
  ([{initial-grid :grid, min-val :min, max-val :max :as problem}, max-tries]
   (loop [grid initial-grid
          steps {}
          tries 0]
     ;This is looping forever 
     (let [best-node (apply max-key (fn [x] (node-heuristic x problem)) (keys grid))]
       ;This is storing the whole world and not just the node
       (println "Best-node: " best-node)
       (cond
         (not (reduce (fn [x y] (or x y)) (vals grid))) (println steps)
         (= tries max-tries) :max-calls-reaches
         :else
         (recur
          (toggle-light-by-list (get-adjacent best-node min-val max-val) grid)
          (toggle-contains best-node steps)
          (+ tries 1))))
     )
   )
  )