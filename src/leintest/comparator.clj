(ns
  ^{:author armatron}
  leintest.comparator)

(require 'clojure.set)

; Comparator class
; Stores a list of objects to compare, their comparitive rankings and which items
; have been compared to one another.
; Methods:
; construct(list)
; all the usual collection methods
; getRanking(item): the ranking of this item
; getRankingCount(item):
; isCompared(item1, item2):
; addComparison(item1, item2, winner)

; Comparator data store
(defstruct comparator-struct :options :scores)

; methods
(defn new-comparator
  "Builds a new comparator"
  [item-sequence]
  (struct comparator-struct (set item-sequence) {})
  )

(defn add-score
  "Adds a choice to the comparator"
  [comparator chosen-val other-val]
  ; Add the choice to the scores for both chosen and other
  (assoc-in
    (assoc-in
      comparator
      [:scores other-val chosen-val]
      0
      )
    [:scores chosen-val other-val]
    1)

  )

(defn !
  "Factorial"
  [x]
  (when-not (integer? x)
    (throw (IllegalArgumentException. "input must be an integer")))
  (when-not (> x 0)
    (throw (IllegalArgumentException. "input must be a positive integer")))
  (apply * (range 1 (+ x 1)))
  )

(defn combinator
  "C(n, k)"
  [n k]
  (/ (apply * (range n (- n k) -1)) (! k))
  )

(defn get-possible-choice-count
  "Gets how many choices can be made from all of the pairs"
  [comparator]
  ; combinator(count(comparator), 2)
  ; combinator(n, k) = n (n-1) ... (n - k - 1) / k!
  ; Try using a macro to build that list.
  ; (apply * (range n (n - k) -1)
  (combinator (count (comparator :options)) 2)
  )

(defn get-current-choices
  "Gets how many choices have been made already"
  [comparator]
  (distinct
    (for [x (keys (get-in comparator [:scores]))
          y (keys (get-in comparator [:scores x]))]
      (hash-set x y)
      )
    )
  )

(defn get-choices-remaining-for
  "Gets remaining choices for value x"
  [comparator x]
  (apply clojure.set/difference
    [(comparator :options), (set (keys (get-in comparator [:scores x]))), #{x}]
    )
  )

(defn get-num-choices-remaining-for
  "Gets remaining number of choices for value x"
  [comparator x]
  (count (get-choices-remaining-for comparator x))
  )

(defn get-num-choices-remaining
  "Gets total remaining choices"
  [comparator]
  ; Get total choices remaining for all options, then divide that by 2
  ; (because each option unused will have a pair)
  (/
    (apply +
      (map
        #(get-num-choices-remaining-for comparator %)
        (comparator :options)
        )
      )
    2)
  )

(defn has-choices-remaining-for?
  "Are there any remaining choices for x?"
  [comparator x]
  ; Get total choices remaining for all options, then divide that by 2
  ; (because each option unused will have a pair)
  (> (get-num-choices-remaining-for comparator x) 0)
  )

(defn has-choices-remaining?
  "Are there any remaining choices?"
  [comparator]
  ; Get total choices remaining for all options, then divide that by 2
  ; (because each option unused will have a pair)
  (> (get-num-choices-remaining comparator) 0)
  )

(defn get-unchosen-pair
  "Retrieves an uncompared pair from the database"
  [comparator]
  ; Choose a random first item in the pair.
  ; Then choose a random item out of get-choices-remaining-for.
  ; If get-choices-remaining-for is empty, then try again.
  (when-not (has-choices-remaining? comparator)
    (throw (IndexOutOfBoundsException. "No more choices left")))
  ; Get a random item with choices remaining only.
  ; That would remove the need for loops
  (let [x (rand-nth (filter #(has-choices-remaining-for? comparator %) (vec (comparator :options))))
        choices (get-choices-remaining-for comparator x)
        ]
      #{x (rand-nth (vec choices))}
    )
  )

(defn get-score-for
  "Gets average score for an item in the comparator"
  [comparator x]
  (let [scores      (vals (get-in comparator [:scores x]))
        score-total (apply + scores)
        score-count (count scores)]
    (if (= 0 score-count)
      nil
      (/ score-total score-count)
      )
    )
  )

(defn- get-score-list
  [comparator]
  (for [option (vec (comparator :options))]
    [option (get-score-for comparator option)]
    )
  )

(defn- score-comparator
  [x y]
  (if (nil? x)
    1
    (if (nil? y)
      -1
      (- y x)
      )
    )
  )

(defn get-scores
  "Gets a list of all average scores in the comparator, highest ranked first, unranked at bottom"
  [comparator]
  (sort-by second score-comparator (get-score-list comparator))
  )
