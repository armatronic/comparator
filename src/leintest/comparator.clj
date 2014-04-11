(ns
  ^{:author armatron}
  leintest.comparator)

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