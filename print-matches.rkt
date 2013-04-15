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

;; only-best-of-dupes : [ListOf Match] -> [ListOf Match]
;;
;; take an initial list of matches, split into lists of duplicates, and choose
;; the best of each list of duplicates.
(define (only-best-of-dupes results)
  (map get-best-match (group-dupes results)))

;; get-best-match : [ListOf Match] -> Match
;;
;; this expects one of the sublists from the output of group-dupes. It will
;; return the member of the list with the smallest "avg-diff"
(define (get-best-match ls)
  (foldl (lambda (x y)
           (if (< (match-avg-diff x)
                  (match-avg-diff y))
               x
               y))
         (first ls)
         (rest ls)))

;; group-dupes : [ListOf Match] -> [ListOf [ListOf Match]]
;;
;; splits the initial list of matches into a list of lists where each
;; sub-list is a list of "duplicate?" matches.
(define (group-dupes results)
  (debug-msg "[group-dupes] results list: ~a\n" results)
  (let loop ((dupe-lists '())
             (results-set (list->set results)))
    (if (set-empty? results-set)
        (begin (debug-msg "[group-dupes] segregated list: ~a\n" dupe-lists)
               dupe-lists)
        (let ((m (set-first results-set))
              (others (set-rest results-set)))
          (let ((dupes (for/set ((o others)
                                 #:when (duplicate? m o))
                         o)))
            (loop (cons (cons m (set->list dupes)) dupe-lists)
                  (set-subtract others dupes)))))))

(define (print-matches results)
  (for-each print-match (only-best-of-dupes results)))
