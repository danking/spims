#lang typed/racket

(require "data-structures.rkt"
         "2htdp-compatibility.rkt"
         2htdp/image)

(provide find-pattern-in-source find-pattern-in-source/tolerance)

;; Match will eventually generate a match struct to then send to the output, right now it gives #t or #f for a match

;; find-pattern-in-source: [Vector [Vector Pixel]]
;;                         [Vector [Vector Pixel]]
;;                         ->
;;                         [Listof [List Number Number]]
;; Match takes a pattern (p) and target (t) vector of vectors of pixels and
;; produces a list of pairs representing the X and Y coordinates of all possible
;; matching coordinates
(: find-pattern-in-source : (Vector (Vector pixel)) (Vector (Vector pixel)) -> (Listof (List Number Number)))
(define (find-pattern-in-source pattern source)
  (find-pattern-in-source/tolerance pattern source 0))

(: find-pattern-in-source/tolerance : (Vector (Vector pixel)) (Vector (Vector pixel)) Number -> (Listof (List Number Number)))
(define (find-pattern-in-source/tolerance pattern source pixel-difference-tolerance)
  (for*/list ((top-left-x (in-range 0 (add1 (- (bitmap-width source) (bitmap-width pattern)))))
              (top-left-y (in-range 0 (add1 (- (bitmap-height source) (bitmap-height pattern)))))
              #:when (find-pattern-in-source-at top-left-x top-left-y
                                                pattern source
                                                pixel-difference-tolerance))
    (list top-left-x top-left-y)))

(: find-pattern-in-source-at : Number Number (Vector (Vector pixel)) (Vector (Vector pixel)) Number -> Boolean)
(define (find-pattern-in-source-at top-left-x top-left-y
                                   pattern source
                                   pixel-difference-tolerance)
  (for*/and ((current-x-of-pattern (in-range 0 (bitmap-width pattern)))
             (current-y-of-pattern (in-range 0 (bitmap-height pattern))))
    (let ((current-x-of-source (+ current-x-of-pattern top-left-x))
          (current-y-of-source (+ current-y-of-pattern top-left-y)))
      (pixels-match-with-tolerance? pattern source
                                    current-x-of-pattern current-y-of-pattern
                                    current-x-of-source current-y-of-source
                                    pixel-difference-tolerance))))
