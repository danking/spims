#lang racket

(provide print-match match-out)

(struct match-out
  ( pattern-img
    source-img
    m1
    n1
    x
    y))

(define (print-match result)
  (print (string-append
          (match-out-pattern-img result)
          " matches "
          (match-out-source-img result)
          " at "
          (number->string (match-out-m1 result))
          "x"
          (number->string (match-out-n1 result))
          "+"
          (number->string (match-out-x result))
          "+"
          (number->string (match-out-y result))))
  (newline))

(define (print-matches results)
  ;; filter matches for duplicates...
  (for-each print-match results))