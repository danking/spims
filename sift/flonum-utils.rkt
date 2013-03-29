#lang typed/racket

(require racket/flonum)

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

