;; Sets namespace. What does gen-class do?
(ns leintest.core
  (:gen-class))

(require '(clojure [string :as s]))
(require 'clojure.pprint)
(use '[clojure.java.io :only (reader)])
;(require 'clojure.java.io :as io)

(defn rand2-nth
  "Returns 2 different random values out of the list."
  [input-sequence]
  (when-not (coll? input-sequence)
    (throw (IllegalArgumentException. "expected a collection")))
  (when (< (count input-sequence) 2)
    (throw (IllegalArgumentException. "expected a collection with at least two items")))
  (if (> (count input-sequence) 2)
    (let [rand1 (rand-nth input-sequence),
          rand2 (rand-nth input-sequence)]
      ; Random values same: try again, otherwise return
      (if (= rand1 rand2)
        (recur input-sequence)
        [rand1 rand2]
        )
      )
    [(first input-sequence) (second input-sequence)]
    )
  )

(defn valid-selection?
  "Validates if input number is between 1 and the length of the list"
  [input-val input-list]
  (try
    (let [int-val (Integer. (re-find  #"\d+" input-val ))]
      (and
        (> int-val 0)
        (<= int-val (count input-list))
        )
      )
    (catch IllegalArgumentException e
      false)
    )
  )

(defn query-for-input [prompt]
  (println prompt)
  (read-line)
  )

(defn get-input-for-line
  "Gets a valid line number selection"
  [item-list]
;  (query-for-input "Enter a decision")
  (loop [input (query-for-input "Enter a decision")]
;  (loop [input (read-line)]
    (if (valid-selection? input item-list)
      (Integer. input)
;      (recur (read-line "Not valid, enter a decision"))
      (recur (query-for-input "Not valid, enter a decision"))
      )
    )
;  (if-let [v (valid-selection? (read-line) item-list)]
;    v
;    (do
;      (println "Invalid")
;      (recur [item-list])
;      )
;    )
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  ; Read a filename from stdin
  ; Offer me a selection of two DIFFERENT random items.
  (with-open [rdr (reader "./.gitignore")]
    (let [item-seq (line-seq rdr),
          rands    (rand2-nth item-seq),
          output   (map-indexed (fn [i line] [(+ 1 i) line]) rands)]
      (doseq [line output]
        (println (str (first line) ": " (second line)))
        )
      ; Now do something with the entry.
      ; Store an entry for my choice against whichever I didn't choose.
      (println (str "You chose \"" (second (nth output (- (get-input-for-line output) 1))) "\""))

      )
    )

;  (loop [input (read-line)]
;    (when-not (= ":done" input)
;      (println (str "You entered: >>" input "<<"))
;      (recur (read-line))))

  ; After that, show me the choice I made.
;  (let [rand1 (rand-nth (keys item-map))])
;  (clojure.pprint/pprint
;    (item-map (rand-nth (keys item-map)))
;    )

;  (let [item-map {}]
;    (doseq [line (line-seq (java.io.BufferedReader. *in*))]
;      (assoc item-map line {}))
;    (print item-map)
;  )
  ; For now, read from stdin, but later take a command line param.
)
