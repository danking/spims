#lang typed/racket

(provide (struct-out histogram) make-hist hist-add)

(: vector-set :
   (All (X)
        [Vectorof X] Natural X -> [Vectorof X]))
;;
;; functional vector update
(define (vector-set vec n val)
  (for/vector: : [Vectorof X]
               ((v (in-vector vec))
                (i (in-naturals)))
    (if (= i n) val v)))

(: hist-add : (case-> (histogram Flonum Flonum -> histogram)
                      (histogram Flonum -> histogram)))
(define (hist-add h val [magnitude 1])
  (match h
    ((histogram v oflow uflow low high bins width)
     (cond
      [(< val low) (histogram v
                              oflow
                              (+ uflow magnitude)
                              low
                              high
                              bins
                              width)]
      [(>= val high) (histogram v
                               (+ oflow magnitude)
                               uflow
                               low
                               high
                               bins
                               width)]
      [else
       (let ((bin (max (exact-floor (/ (- val low) width))
                       0)))
         (histogram (vector-set v bin
                                (+ (vector-ref v bin)
                                   magnitude))
                    oflow
                    uflow
                    low
                    high
                    bins
                    width))]))))

(: hist-ref : histogram Natural -> Flonum)
(define (hist-ref h n)
  (match h
    ((histogram v _ _ _ _ _ bins)
     (unless (and (<= 0 n)
                  (< n bins))
       (error 'hist-ref
              "bin index out of range [0,~a) : ~a"
              bins n))

     (vector-ref v n))))

(: make-hist : Flonum Flonum Natural -> histogram)
(define (make-hist low high bins)
  (histogram (make-vector bins #i0.0)
             #i0.0
             #i0.0
             low
             high
             bins
             (/ (- high low) bins)))

(struct: histogram
         ([v : [Vectorof Flonum]]
          [overflow : Flonum]
          [underflow : Flonum]
          [low : Flonum]
          [high : Flonum]
          [bins : Natural]
          [width : Flonum])
         #:transparent)
