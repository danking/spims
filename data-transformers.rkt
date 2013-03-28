#lang racket

(require math/array
         math/matrix
         "data-structures.rkt")

(provide map-bitmap fold-bitmap pixel->grayscale)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-transformers.rkt
;;
;; Procedures to do iteration or recursion over the data structures.

;; map-bitmap : [Pixel -> Pixel] ImageBitmap -> ImageBitmap
;;
;; A map procedure for the ImageBitmap datatype.
(define (map-bitmap transformer bitmap)
  (matrix-map transformer bitmap))

;; fold-bitmap : ∀TYPE1, TYPE2.
;;                    [Pixel TYPE1 -> TYPE1]
;;                    [TYPE1 TYPE2 -> TYPE2]
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
(define (fold-bitmap pixel-transformer row-transformer
                     pixel-base row-base
                     bitmap)
  (for/fold ((row-accumulator row-base))
            ((row (in-array-axis bitmap)))
    (row-transformer (for/fold ((pixel-accumulator pixel-base))
                               ((pixel (in-array row)))
                       (pixel-transformer pixel pixel-accumulator))
                     row-accumulator)))

;; pixel->grayscale : [Pixel] -> Number]

;; Returns the grayscale value for an RGB pixel
;; (R * 11 + G * 16 + B * 5) / 32
(define (pixel->grayscale p)
  (/ (+ (* (pixel-red p) 11) (* (pixel-green p) 16) (* (pixel-blue p) 5)) 32))

