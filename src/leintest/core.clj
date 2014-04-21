;; Sets namespace. What does gen-class do?
(ns leintest.core
  (:gen-class)
  (:require [leintest.comparator :as c])
  (:use [clojure.java.io :only (reader)])
;  (:use [clojure.pprint])
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
  "Prompts for input from stdin"
  (println prompt)
  (read-line)
  )

(defn- print-scores
  "Outputs scores for this comparator"
  [comparator]
  (doseq [[item score] (c/get-scores comparator)]
    (println (str item ": " score))
    )
  ; Return -1 to loop thru again
  -1
  )

(defn get-input-for-line
  "Gets a valid line number selection."
  [item-list comparator]
  (loop [input (query-for-input "Choose one: ")]
    (when (= input "s")
      (print-scores comparator)
      )
    (when (= input "?")
      (println "?:    help")
      (println "1, 2: a choice")
      (println "s:    current scores")
      (println "x:    exit")
      )
    (when (= input "x")
      -1)
    (if (valid-selection? input item-list)
      (Integer. input)
      ; Inputs:
      ; 1, 2: a choice
      ; s: current scores
      ; x: exit
      ; ?: help
      (recur (query-for-input "Choose one: "))
      )
    )
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  ; Offer me a selection of two DIFFERENT random selections from this file.
  (with-open [rdr (reader "./SAMPLE")]
    ; Keep going until I say stop, or until all are chosen
    (loop [comparator (c/new-comparator (line-seq rdr))]
      ; Get a pair out of the
      (let [pair   (c/get-unchosen-pair comparator)
            output (map-indexed (fn [i line] {:index (+ 1 i), :line line}) pair)]
        ; Output the line (intersperse with map)
        (doseq [line output]
          (println (str (line :index) ": " (line :line)))
          )
        ; Now do something with the entry.
        ; Store an entry for my choice against whichever I didn't choose.
        ; Selection will either be 0 or 1
        (let [selection (get-input-for-line output comparator)]
          ; selection = -1: exit
          (when-not (= selection -1)
            (let [selected-line  (nth output (- selection 1))
                  other-line     (first (concat (take (- selection 1) output) (nthrest output selection)))
                  new-comparator (c/add-score comparator (selected-line :line) (other-line :line))]
              (if (c/has-choices-remaining? new-comparator)
                (recur new-comparator)
                ; Output totals
                (doseq [[item score] (c/get-scores comparator)]
                  (println (str item ": " score))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
