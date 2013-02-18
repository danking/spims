#lang racket

(require "data-structures.rkt"
         "2htdp-compatibility.rkt"
         "print-matches.rkt"
         2htdp/image)

(provide match match-pattern valid-pixel? pixels-match-with-tolerance?)

;; Match will eventually generate a match struct to then send to the output, right now it gives #t or #f for a match


;;(vector (vector pixel)) (vector (vector pixel)) -> (Listof Match)
;; Match takes a pattern (p) and target (t) vector of vectors of pixels and returns true if p exists in t
(define (match p t p-filename t-filename)
  ;; Tolerance for JPEGs
  (define tol 50)
  ;; Some useful information from our images
  (define p-rows (vector-length p))
  (define p-columns (vector-length (vector-ref p 0)))
  (define t-rows (vector-length t))
  (define t-columns (vector-length (vector-ref t 0)))
  ;; Begins pattern matching at 0 0 in both p and t
  (match-pattern p t 0 0 0 0 p-rows p-columns t-rows t-columns tol p-filename t-filename))

;; (vector (vector pixel)) (vector (vector pixel)) int int int int int int int -> (Listof Match)
;; Match-pattern takes the p, t, the x,y location in both p and t as well as the image widths/lengths
;; Produces a boolean if p and t match.

(define (match-pattern p t px py tx ty p-rows p-columns t-rows t-columns tol p-filename t-filename)
  ;; Is the pixel valid and are the pixels of the two images the same color?
  (if (and (valid-pixel? px py tx ty p-rows p-columns t-rows t-columns) (pixels-match-with-tolerance? p t px py tx ty tol))
      (cond
        ;;Pattern has been entirely checked, match confirmed!
        [(and (>= px (- p-columns 1)) (>= py (- p-rows 1)))
         (print-match (match-out (get-path-filename p-filename) (get-path-filename t-filename) p-columns p-rows (- tx px) (- ty py)))]
        ;;Pattern has reached the end of the line but continues on the next line
        [(and (= px (- p-columns 1)) (< py (- p-rows 1)))
         (match-pattern p t (- px (- p-columns 1)) (+ 1 py) (- tx (- p-columns 1)) (+ 1 ty) p-rows p-columns t-rows t-columns tol p-filename t-filename)]
        ;;Continue checking by recursing one pixel to the right
        [else (match-pattern p t (+ px 1) py (+ tx 1) ty p-rows p-columns t-rows t-columns tol p-filename t-filename)])
      (cond
        ;;Each pixel in target has been checked and do not return a match
        [(and (>= tx (- t-columns 1)) (>= ty (- t-rows 1))) (void)]
        ;;Restart match-pattern at the beginning of the next row
        [(>= tx (- t-columns 1)) (match-pattern p t 0 0 0 (+ ty 1) p-rows p-columns t-rows t-columns tol p-filename t-filename)]
        ;;Else move one right of the patterns upper left corner
        [else (match-pattern p t 0 0 (+ 1 (- tx px)) (- ty py) p-rows p-columns t-rows t-columns tol p-filename t-filename)])))


;; takes current pixel positions and image information and returns true if the pixel falls within the target.
(define (valid-pixel? px py tx ty p-rows p-columns t-rows t-columns)
  (and
   (<= ty (- t-rows 1))
   (<= tx (- t-columns 1))
   (<= py (- p-rows 1))
   (<= px (- p-columns 1))
   (<= px tx)
   (<= py ty)
   (<= p-rows t-rows)
   (<= p-columns t-columns)))

;; takes the filepath and produces the filename at the end of the path
(define (get-path-filename filepath)
  (let-values (((a b c)
                (split-path filepath)))
    (path->string b)))
