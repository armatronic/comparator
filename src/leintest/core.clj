;; Sets namespace. What does gen-class do?
(ns leintest.core
  (:gen-class)
  (:require [leintest.comparator :as c])
  (:use [clojure.java.io :only (reader)])
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

(defn get-input-for-line
  "Gets a valid line number selection"
  [item-list]
  (loop [input (query-for-input "Enter a decision")]
    (if (valid-selection? input item-list)
      (Integer. input)
      (recur (query-for-input "Not valid, enter a decision"))
      )
    )
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  ; Offer me a selection of two DIFFERENT random selections from this file.
  (with-open [rdr (reader "./.gitignore")]
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
        (let [selection     (get-input-for-line output)
              selected-line (nth output (- selection 1))
              other-line    (first (concat (take (- selection 1) output) (nthrest output selection)))
              ]
          (println (str "You chose \"" (selected-line :line) "\" (not \"" (other-line :line) "\")"))
          )
        ; Do you want to choose again?
;        (recur [(c/add-score (first )])])
        )
      )
;    (print comparator)
    )
  )
