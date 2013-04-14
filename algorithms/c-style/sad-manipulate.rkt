#lang racket

(require "sad-data.rkt"
         "../../data-structures.rkt")

(provide sad-filter sad-less-than
         (struct-out sad)
         sad-max sad-min
         sad-mean sad-variance)

(define (sad-filter sad proc)
  (for*/list ((x (in-range 0 (sad-w sad)))
              (y (in-range 0 (sad-h sad)))
              #:when (proc (sad-get sad x y)))
    (pre-match x y (sad-get sad x y))))

(define (sad-max sad)
  (stats-max (sad-stats sad)))

(define (sad-min sad)
  (stats-min (sad-stats sad)))

(define (sad-mean sad)
  (or (stats-mean (sad-stats sad))
      (begin (set-stats-mean! (sad-stats sad) (calculate-mean sad))
             (stats-mean (sad-stats sad)))))

(define (sad-variance sad)
  (or (stats-variance (sad-stats sad))
      (begin (set-stats-variance! (sad-stats sad) (calculate-variance sad))
             (stats-variance (sad-stats sad)))))

(define (calculate-mean sad)
  (let ((sum (for*/sum ((x (in-range 0 (sad-w sad)))
                        (y (in-range 0 (sad-h sad))))
               (sad-get sad x y))))
    (/ sum (* (sad-w sad) (sad-h sad)))))

(define (calculate-variance sad)
  (let ((mean (sad-mean sad)))
    (/ (for*/sum ((x (in-range 0 (sad-w sad)))
                  (y (in-range 0 (sad-h sad))))
         (sqr (- (sad-get sad x y) mean)))
       (sub1 (* (sad-w sad) (sad-h sad))))))

(define (sad-less-than sad val)
  (sad-filter sad (lambda (x) (< x val))))

(define (sad-get s x y)
  (let ((bs (sad-bs s))
        (w (sad-w s)))
   (+ (bytes-ref bs (+ (* x 4) (* y w 4) 0))
      (arithmetic-shift (bytes-ref bs (+ (* x 4) (* y w 4) 1)) 8)
      (arithmetic-shift (bytes-ref bs (+ (* x 4) (* y w 4) 2)) 16)
      (arithmetic-shift (bytes-ref bs (+ (* x 4) (* y w 4) 3)) 24))))

(define (sad-get/bytes s x y)
  (let ((bs (sad-bs s))
        (w (sad-w s)))
    (list (bytes-ref bs (+ (* x 4) (* y w 4) 0))
          (bytes-ref bs (+ (* x 4) (* y w 4) 1))
          (bytes-ref bs (+ (* x 4) (* y w 4) 2))
          (bytes-ref bs (+ (* x 4) (* y w 4) 3)))))
