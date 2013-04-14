#lang racket

(require "data-structures.rkt"
         "logging.rkt")

(provide print-match print-matches)

(define LOWER-SIZE-TOLERANCE 1.2)   ;; Used when we have scaling
(define UPPER-SIZE-TOLERANCE .8)    ;; Used when we have scaling
(define LOCATION-INTOLERANCE 2)
(define (pythagorean a b)
  (sqrt (+ (expt a 2) (expt b 2))))

(define (print-match result)
  (display (string-append
            (get-path-filename (match-pattern-img result))
            " matches "
            (get-path-filename (match-source-img result))
            " at "
            (number->string (match-m1 result))
            "x"
            (number->string (match-n1 result))
            "+"
            (number->string (match-x result))
            "+"
            (number->string (match-y result))
            (if (debug)
                (format " (with ~a average-difference)" (match-avg-diff result))
                "")))
  (newline))

;; takes the filepath and produces the filename at the end of the path
(define (get-path-filename filepath)
  (let-values (((a b c)
                (split-path filepath)))
    (path->string b)))


(define (duplicate? m1 m2)
  (and (same-images m1 m2)
       (same-size m1 m2)
       (same-loc m1 m2)))


(define (same-images m1 m2)
  (and (string=? (get-path-filename (match-pattern-img m1)) (get-path-filename  (match-pattern-img m2)))
       (string=? (get-path-filename (match-source-img m1)) (get-path-filename  (match-source-img m2)))))


(define (same-size m1 m2) #t)
#| We aren't going to need this until scaling starts giving us different size matches
  (define size ratio (/ (* (match-m1 m1) (match-n1 m1))
                        (* (match-m1 m2) (match-n1 m2))))
  (and (< ratio UPPER-SIZE-TOLERANCE)
       (> ratio LOWER-SIZE-TOLERANCE))|#

(define (same-loc m1 m2)
  (< (pythagorean (- (match-x m1) (match-x m2))
                  (- (match-y m1) (match-y m2)))
     (/ (pythagorean (match-m1 m1)
                     (match-n1 m1))
        LOCATION-INTOLERANCE)))

(define (simple-filter results)
  (reverse (foldl (lambda (x lst)
                    (cons x (filter-not (curry duplicate? x) lst)))
                  '()
                  results)))

(define (print-matches results)
  (for-each print-match (simple-filter results)))
