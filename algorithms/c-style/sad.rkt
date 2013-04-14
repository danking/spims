#lang racket

(require "ffi.rkt"
         "sad-manipulate.rkt"
         "../../data-structures.rkt")

(provide c-style)

(define (c-style pat src)
  (let* ((sad (generate-sad-values pat src))
         (min (pre-match-avg-diff (sad-min sad)))
         (max (pre-match-avg-diff (sad-max sad)))
         (range (- max min))
         (105% (* range 1.05)))
    (sad-filter sad (lambda (val) (< val 105%)))))
