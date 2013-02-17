#lang racket

(provide print-match)

(struct match
  ( pattern-img
    source-img
    m1
    n1
    x
    y))

(define (print-match result)
  (print (string-append
          (match-pattern-img result)
          " matches "
          (match-source-img result)
          " at "
          (number->string (match-m1 result))
          "x"
          (number->string (match-n1 result))
          "+"
          (number->string (match-x result))
          "+"
          (number->string (match-y result))))
  (newline))

(define (print-matches results)
  ;; filter matches for duplicates...
  (for-each print-match results))