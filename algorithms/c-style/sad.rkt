#lang racket

(require "ffi.rkt"
         "sad-manipulate.rkt"
         "../../data-structures.rkt"
         "../../logging.rkt")

(provide c-style)

;; flower.gif matches ac1000.jpg with a average sad value of ~43.729
(define MAXIMUM_AVERAGE_SAD 44)
(define TOLERABLE_RANGE_FROM_MIN 0.05)

(define (c-style pat src)
  (let* ((sad (cond [(debug)
                     (debug-msg "generate-sad-values\n")
                     (time (generate-sad-values pat src))]
                    [else (generate-sad-values pat src)]))
         (min (pre-match-avg-diff (sad-min sad)))
         (max (pre-match-avg-diff (sad-max sad)))
         (range (- max min))
         (5-percent (let ((v (* range TOLERABLE_RANGE_FROM_MIN)))
                      (if (nan? v)
                          (begin0 0
                            (debug-msg
                             "[sad] 5-percent was nan, set it to zero\n"))
                          v)))
         (tolerance (+ min 5-percent)))
    (debug-msg "range ~a, TOLERABLE_RANGE_FROM_MIN ~a, ~a\n"
               range TOLERABLE_RANGE_FROM_MIN
               (* range TOLERABLE_RANGE_FROM_MIN))
    (debug-msg "min ~a, 5-percent ~a\n" min 5-percent)
    (debug-msg "max sad: ~a\n" (* (send pat get-width)
                                  (send pat get-height)
                                  MAXIMUM_AVERAGE_SAD))
    (if (< min
           (* (send pat get-width)
              (send pat get-height)
              MAXIMUM_AVERAGE_SAD))
        (cond [(debug)
               (time (sad-filter sad
                                 (lambda (val)
                                   (debug-msg
                                    "[c-style/sad-filter] (<= ~a ~a) = ~a\n"
                                    val tolerance (<= val tolerance))
                                   (<= val tolerance))))]
              [else (sad-filter sad (lambda (val) (<= val tolerance)))])
        empty)))
