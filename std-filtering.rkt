#lang racket

(require "data-structures.rkt")

;; sorts a list of pre-matches by ascending avg-diff
(define (sort-pre-matches lop)
  (sort lop (λ (x y) (< (pre-match-avg-diff x) (pre-match-avg-diff y)))))

;; finds the mean avg-diff of a list of pre-matches
(define (mean lop)
  (/ (foldl (λ (p acc) (+ (pre-match-avg-diff p) acc)) 0 lop) (length lop)))

;; finds the standard deviation of a list of pre-matches given a mean avg
(define (std lop avg)
  (sqrt (/ (foldl (λ (p acc) (+ (sqr (- (pre-match-avg-diff p) avg)) acc)) 0 lop) (length lop))))

;; filters a list of pre-matches by the standard deviation of that list
(define (std-filter lop)
  (let* ((mean (mean lop))
         (std (std lop mean)))
    (filter (λ (p) (< (+ (pre-match-avg-diff p) std std) mean)) lop)))


#|(define list1 (list (pre-match 0 0 100)
                   (pre-match 0 0 200)
                   (pre-match 0 0 0)
                   (pre-match 0 0 50)
                   (pre-match 0 0 100)
                   (pre-match 0 0 200)
                   (pre-match 0 0 0)
                   (pre-match 0 0 100)
                   (pre-match 0 0 200)
                   (pre-match 0 0 0)
                   (pre-match 0 0 100)
                   (pre-match 0 0 200)
                   (pre-match 0 0 0)
                   (pre-match 0 0 100)
                   (pre-match 0 0 200)
                   (pre-match 0 0 0)
                   (pre-match 0 0 100)
                   (pre-match 0 0 200)
                   (pre-match 0 0 0)))

;; tests
(sort-pre-matches list1)
(mean list1)
(std list1 (mean list1))

|#
     
     