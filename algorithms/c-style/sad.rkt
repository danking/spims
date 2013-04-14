#lang racket

(require "ffi.rkt"
         "sad-manipulate.rkt"
         "../../data-structures.rkt"
         "../../logging.rkt")

(provide c-style)

;; flower.gif matches ac1000.jpg with a average sad value of ~43.729
(define MAXIMUM_AVERAGE_SAD 44)
(define TOELRABLE_RANGE_FROM_MIN 0.05)

(define (c-style pat src)
  (let* ((sad (generate-sad-values pat src))
         (min (pre-match-avg-diff (sad-min sad)))
         (max (pre-match-avg-diff (sad-max sad)))
         (range (- max min))
         (5% (* range TOELRABLE_RANGE_FROM_MIN))
         (tolerance (+ min 5%)))
    (debug-msg "max sad: ~a" (* (send pat get-width)
                                (send pat get-height)
                                MAXIMUM_AVERAGE_SAD))
    (if (< min
           (* (send pat get-width)
              (send pat get-height)
              MAXIMUM_AVERAGE_SAD))
        (sad-filter sad (lambda (val) (< val tolerance)))
        empty)))
