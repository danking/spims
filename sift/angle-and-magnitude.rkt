#lang typed/racket

(require images/flomap
         racket/flonum
         "flonum-utils.rkt"
         "../data-structures.rkt")
(provide m theta θ)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Angle and Magnitude

(: m : flomap Integer Integer -> Nonnegative-Real)
;;
;; computes the "magnitude" of a given flomap component according to page 13 of
;; the SIFT document
(define (m fm x y)
  (let ((L (lambda: ((x : Integer) (y : Integer))
             (flomap-ref* fm x y))))
    (sqrt (+ (sqr (euclidean-distance (L (add1 x) y)
                                      (L (sub1 x) y)))
             (sqr (euclidean-distance (L x (add1 y))
                                      (L x (sub1 y))))))))

(: theta : flomap Integer Integer -> Real)
;;
;; computes the "angle" of a given flomap component according to page 13 of the
;; SIFT document
(define (theta fm x y)
  (let ((L (lambda: ((x : Integer) (y : Integer))
             (flomap-ref* fm x y))))
    (atan (/ (euclidean-distance (L x (add1 y))
                                 (L x (sub1 y)))
             (euclidean-distance (L (add1 x) y)
                                 (L (sub1 x) y))))))
(define θ theta)
