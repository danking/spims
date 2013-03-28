#lang typed/racket

(require images/flomap
         racket/flonum
         "data-structures.rkt")
(provide m theta θ)

(: euclidean-distance : FlVector FlVector -> Nonnegative-Real)
;;
;; computes the euclidean distance between two flvectors of arbitrary (but
;; equivalent) dimensionality
(define (euclidean-distance us vs)
  (unless (= (flvector-length us)
             (flvector-length vs))
    (error 'euclidean-distance
           "Vectors must have same dimensions."))

  (sqrt (for/sum: : Nonnegative-Real ((u (in-flvector us))
                                      (v (in-flvector vs)))
          (sqr (- u v)))))

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
