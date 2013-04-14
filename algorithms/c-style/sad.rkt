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
         (5% (* range 0.05))
         (tolerance (+ min 5%)))
    (sad-filter sad (lambda (val) (< val tolerance)))))
