#lang racket

(provide (all-defined-out))

(struct sad (bs w h stats) #:transparent)

;; max is a Pre-Match
;; min is a Pre-Match
;; arith-mean is a [Maybe Number]
;; variance is a [Maybe Number]
(struct stats (max min (mean #:mutable) (variance #:mutable)) #:transparent)

