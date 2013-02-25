#lang typed/racket

(require "data-structures.rkt")

(provide print-match print-matches)

(: print-match : match -> Void)
(define (print-match result)
  (display (string-append
            (get-path-filename (match-pattern-img result))
            " matches "
            (get-path-filename (match-source-img result))
            " at "
            (number->string (match-m1 result))
            "x"
            (number->string (match-n1 result))
            "+"
            (number->string (match-x result))
            "+"
            (number->string (match-y result))))
  (newline))

;; takes the filepath and produces the filename at the end of the path
(: get-path-filename : Path -> String)
(define (get-path-filename filepath)
  (let-values (((a b c)
                (split-path filepath)))
    (path->string b)))

(: print-matches : (Listof match) -> (Listof match))
(define (print-matches results)
  ;; filter matches for duplicates...
  (for-each print-match results))
