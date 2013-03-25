#lang racket

(require rackunit
         "../data-transformers.rkt"
         "../data-structures.rkt"
         "test-utils.rkt")

(provide data-transformers-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests/data-transformers.rkt

(define data-transformers-tests
  (test-suite
   "Tests for data-transformers.rkt"

   (test-suite
    "map-bitmap"

    (test-equal? "red to green"
                 (map-bitmap (lambda (p) (pixel 0 255 0))
                             (create-bitmap 10 10 (lambda (i j) (pixel 255 0 0))))
                 (create-bitmap 10 10 (lambda (i j) (pixel 0 255 0))))
    (test-equal? "inversion"
                 (map-bitmap (lambda (p) (pixel (- 255 (pixel-red p)) 0 0))
                             (create-bitmap 10 10 (lambda (i j) (pixel i 0 0))))
                 (create-bitmap 10 10 (lambda (i j) (pixel (- 255 i) 0 0)))))

   (test-suite
    "fold-bitmap"

    (test-case
     "compute pixel averages"

     ;; accumulate a triplet of the sums of the red, green, and blue channels
     (define (sum-pixels p sum)
       (map +
            (list (pixel-red p) (pixel-green p) (pixel-blue p))
            sum))
     (define (sum-rows row sum)
       (map + row sum))
     (define (pixel-averages bitmap)
       (map /
            (fold-bitmap sum-pixels sum-rows (list 0 0 0) (list 0 0 0) bitmap)
            (let ((size (* (bitmap-width bitmap) (bitmap-height bitmap))))
              (list size size size))))

     (check-equal?
      (pixel-averages (create-bitmap 10 10 (lambda (i j)
                                             (pixel 255 0 0))))
      (list 255 0 0))
     (check-equal?
      (pixel-averages (create-bitmap 10 10 (lambda (i j)
                                             (pixel i (* i j) (+ i j)))))
      ;; be careful to use exact numbers here (i.e. #eNUMBER or fractions),
      ;; otherwise, racket will use exact numbers to calculate this and you'll
      ;; be confused when things that appear equal are not equal.
      (list 9/2 81/4 9))))))
