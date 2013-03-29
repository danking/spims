#lang typed/racket

(provide normal-distribution)

(: normal-distribution : (case-> (Flonum Flonum -> (Flonum -> Flonum))
                                 (Flonum -> (Flonum -> Flonum))
                                 (-> (Flonum -> Flonum))))
;;
;; produces a normal distribution centered at μ and with a standard deviation of
;; σ
(define (normal-distribution [σ 1] [μ 0])
  (lambda (x)
    (/ (exp (- (/ (sqr (- x μ))
                  (* 2 (sqr σ)))))
       σ (sqrt (* 2 pi)))))
