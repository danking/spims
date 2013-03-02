#lang racket

(require "data-structures.rkt"
         "2htdp-compatibility.rkt"
         2htdp/image)

(provide find-pattern-in-source find-pattern-in-source/tolerance)

;; find-pattern-in-source: [Vector [Vector Pixel]]
;;                         [Vector [Vector Pixel]]
;;                         ->
;;                         [Listof [List Number Number]]
;; Match takes a pattern (p) and target (t) vector of vectors of pixels and
;; produces a list of pairs representing the X and Y coordinates of all possible
;; matching coordinates
(define (find-pattern-in-source pattern source)
  (find-pattern-in-source/tolerance pattern source 0))

(define (find-pattern-in-source/tolerance pattern source pixel-difference-tolerance)
  (for*/list ((top-left-x (in-range 0 (add1 (- (bitmap-width source)
                                               (bitmap-width pattern)))))
              (top-left-y (in-range 0 (add1 (- (bitmap-height source)
                                               (bitmap-height pattern)))))
              ;; this is hack so that the avg-diff binding is available in the
              ;; body of the for*/list clause. `in-value' takes the given value
              ;; and returns a sequence of length one, containing only the
              ;; given value
              (avg-diff (in-value
                         (average-pixel-difference/short-circuiting top-left-x top-left-y
                                                                    pattern source
                                                                    pixel-difference-tolerance)))
              ;; if the avg-diff is false, then don't execute the body
              #:when (begin (when (and (debug) (zero? (modulo top-left-x 300)))
                              (printf "  at (~a,~a), tolerance ~a, avg-diff ~a\n"
                                      top-left-x top-left-y pixel-difference-tolerance
                                      avg-diff))
                            avg-diff))
    (pre-match top-left-x top-left-y avg-diff)))

;; average-pixel-difference/short-circuiting : Natural Natural
;;                                             ImageBitmap ImageBitmap
;;                                             Number
;;                                             ->
;;                                             (U #f Number)
;;
;; Computes the average difference between pixels of the pattern image
;; overlaid on the source image at the given offset. If the average becomes
;; larger than the tolerance at any point, then we short-circuit and return #f.
(define (average-pixel-difference/short-circuiting top-left-x top-left-y
                                                   pattern source
                                                   average-difference-tolerance)
  (define (tolerable-average? average)
    (< average average-difference-tolerance))
  (define pattern-size (* (bitmap-width pattern)
                          (bitmap-height pattern)))
  (let ((difference
         (for*/fold
             ((total-difference 0))
             ((current-x-of-pattern (in-range 0 (bitmap-width pattern)))
              (current-y-of-pattern (in-range 0 (bitmap-height pattern)))
              ;; if the total difference was set to #f in the last iteration, stop
              #:break (false? total-difference))
           (let* ((current-x-of-source (+ current-x-of-pattern top-left-x))
                  (current-y-of-source (+ current-y-of-pattern top-left-y))
                  (current-difference (pixel-difference-in-bitmap-at
                                       pattern source
                                       current-x-of-pattern current-y-of-pattern
                                       current-x-of-source current-y-of-source))
                  (new-total (+ total-difference current-difference))
                  (new-average (/ new-total pattern-size)))
             (and (tolerable-average? new-average) new-total)))))
    (and difference (/ difference pattern-size))))
