#lang typed/racket

(require "data-structures.rkt")

(provide map-bitmap fold-bitmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-transformers.rkt
;;
;; Procedures to do iteration or recursion over the data structures.

;; map-bitmap : [Pixel Number Number -> Pixel] ImageBitmap -> ImageBitmap
;;
;; A map procedure for the ImageBitmap datatype. The transformer should expect
;; the x and y coordinates of the pixel as the second and third arguments,
;; respectively.
(: map-bitmap : (pixel Number Number -> pixel) (Vector (Vector pixel)) -> (Vector (Vector pixel)))
(define (map-bitmap transformer bitmap)
  (for/vector (((row y) (in-indexed bitmap)))
    (for/vector (((element x) (in-indexed row)))
      (transformer element x y))))

;; fold-bitmap : ∀TYPE1, TYPE2.
;;                    [Pixel Number Number TYPE1 -> TYPE1]
;;                    [TYPE1 Number TYPE2 -> TYPE2]
;;                    TYPE1
;;                    TYPE2
;;                    ImageBitmap
;;                    ->
;;                    TYPE2
;;
;; A left fold (catamorphism) for the ImageBitmap datatype. The
;; pixel-transformer should expect the x and y coordinates of the pixel as the
;; second and third arguments. The row-transformer should expect the x
;; coordinate of the row as the second argument. All coordinates are measured in
;; pixel coordinates, i.e., the origin (0, 0) is located at the top-left corner.
;;
;; NB: Left folds are more space efficient than right folds, but process the
;; elements in order (as opposed to right folds, which process the elements in
;; reverse order), which means that (foldl cons '() '(1 2 3)) does not equal '(1
;; 2 3).
(: fold-bitmap : (pixel Number Number A -> A) (A Number B -> B) A B (Vector (Vector pixel)) -> B)
(define (fold-bitmap pixel-transformer row-transformer
                     pixel-base row-base
                     bitmap)
  (for/fold ((row-accumulator row-base))
            (((row y) (in-indexed bitmap)))
    (row-transformer (for/fold ((pixel-accumulator pixel-base))
                               (((pixel x) (in-indexed row)))
                       (pixel-transformer pixel x y pixel-accumulator))
                     y
                     row-accumulator)))

