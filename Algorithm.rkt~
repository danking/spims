#lang racket

(require 2htdp/image
         picturing-programs)

(provide match match-pattern valid-pixel? pixels-match?)

;; Match will eventually generate a match struct to then send to the output, right now it gives #t or #f for a match


;;(vector (vector color)) (vector (vector color)) -> Boolean
;; Match takes a pattern (p) and target (t) vector of vectors of colors and returns true if p exists in t
(define (match p t)
  ;; Some useful information from our images
  (define p-rows (vector-length p))
  (define p-columns (vector-length (vector-ref p 0)))
  (define t-rows (vector-length t))
  (define t-columns (vector-length (vector-ref t 0)))
  ;; Begins pattern matching at 0 0 in both p and t
  (match-pattern p t 0 0 0 0 p-rows p-columns t-rows t-columns))

;;(vector (vector color)) (vector (vector color)) int int int int int int int -> Boolean
;; Match-pattern takes the p, t, the x,y location in both p and t as well as the image widths/lengths
;; Produces a boolean if p and t match.

(define (match-pattern p t px py tx ty p-rows p-columns t-rows t-columns)
  ;; Is the pixel valid and are the pixels of the two images the same color?
  (if (and (valid-pixel? px py tx ty p-rows p-columns t-rows t-columns) (pixels-match? p t px py tx ty))
      (cond
        ;;Pattern has been entirely checked, match confirmed!
        [(and (= px p-columns) (= py p-rows)) #t]
        ;;Pattern has reached the end of the line but continues on the next line
        [(and (= px p-columns) (< py p-rows))
         (match-pattern p t (- px p-columns) (+ 1 py) (- tx p-columns) (+ 1 ty) p-rows p-columns t-rows t-columns)]
        ;;Continue checking by recursing one pixel to the right
        [else (match-pattern p t (+ px 1) py (+ tx 1) ty p-rows p-columns t-rows t-columns)])
      (cond
        ;;Each pixel in target has been checked and do not return a match
        [(and (= tx t-columns) (= ty t-rows)) #f]
        ;;Restart match-pattern at the beginning of the next row
        [(= tx t-columns) (match-pattern p t 0 0 0 (+ ty 1) p-rows p-columns t-rows t-columns)]
        ;;Else move one right of the patterns upper left corner
        [else (match-pattern p t 0 0 (+ 1 (- tx px)) (- ty py) p-rows p-columns t-rows t-columns)])))


;; takes current pixel positions and image information
(define (valid-pixel? px py tx ty p-rows p-columns t-rows t-columns)
  (and (<= px tx)
       (<= py ty)
       (<= p-rows t-rows)
       (<= p-columns t-columns)
       (<= ty t-rows)
       (<= tx t-columns)))

;; references the color within the pattern and target and checks if their color elements are equal
(define (pixels-match? p t px py tx ty)
  (color=? (vector-ref (vector-ref p py) px)
           (vector-ref (vector-ref t ty) tx)))


