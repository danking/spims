#lang racket

(require "data-structures.rkt")

(provide print-match print-matches)

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
            (number->string (match-y result))))
  (newline))

;; takes the filepath and produces the filename at the end of the path
(define (get-path-filename filepath)
  (let-values (((a b c)
                (split-path filepath)))
    (path->string b)))


(define (duplicate? m1 m2)
  (if (and (same-images m1 m2)
           (same-size m1 m2))
      (same-loc m1 m2)
      #f))


(define (same-images m1 m2)
  (and (string=? (get-path-filename (match-pattern-img m1)) (get-path-filename  (match-pattern-img m2)))
       (string=? (get-path-filename (match-source-img m1)) (get-path-filename  (match-source-img m2)))))
 

(define (same-size m1 m2)
  (and (< (- (match-m1 m1) (match-m1 m2)) 3)
       (< (- (match-n1 m1) (match-n1 m2)) 3)))
  

(define (same-loc m1 m2)
  (< (sqrt (+ (exp (- (match-x m1) (match-x m2)) 2)
              (exp (- (match-y m1) (match-y m2)) 2)))
     (/ (sqrt (+ (exp (* 2 (match-m1 m1)) 2)
                 (exp (* 2 (match-n1 m1)) 2)))
        50)))


;(define (structure-by-image results prev-pattern prev-source)
;  (cond [(empty? results) empty]
;        [(= ((first results)cons (filter results


;(define (filter-matches results)
;  (for-each organized-filter (structure-by-image results "" "")))



(define (simple-filter results)
  (cond [(empty? results) empty]
        [(empty? (rest results)) results]
        [else (define r1 (first results))
              (define (f x) (not (duplicate? r1 x)))
              (cons r1 (simple-filter (filter f (rest results))))]))

(define (print-matches results)
  (define filtered-results (simple-filter results))
  (for-each print-match filtered-results))
