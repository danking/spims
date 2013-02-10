#lang racket

(provide pixel pixel-red pixel-blue pixel-green
         get-pixel-at bitmap-width bitmap-height
         create-bitmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-structures.rkt
;;
;; Data definitions and data structures

;; A pixel structure represents a pixel in an image. Every pixel has a bit-depth
;; of 32 (because of the racket/draw library), which means each color channel is
;; at most 8 bytes of data.
;;
;; (pixel Number Number Number Number)
(struct pixel (red green blue) #:transparent)

;; An ImageBitmap is a [Vector [Vector Pixel]]
;; To get the element in the i-th row and the j-th column we execute:
;;
;;   (vector-ref (vector-ref bitmap i) j)
;;
;; in pieces, this is:
;;
;;   (let* ((ith-row (vector-ref bitmap i))
;;          (jth-column-of-ith-row (vector-ref ith-row j)))
;;     jth-column-of-ith-row)
;;
;; However, the vector-ref procedure shouldn't be used directly.

;; get-pixel-at : ImageBitmap Number Number -> Pixel
(define (get-pixel-at bitmap row column)
  (vector-ref (vector-ref bitmap row) column))

;; bitmap-height : ImageBitmap -> Number
(define (bitmap-height bitmap)
  (vector-length bitmap))

;; bitmap-width : ImageBitmap -> Number
(define (bitmap-width bitmap)
  (if (> (bitmap-height bitmap) 0)
      (vector-length (vector-ref bitmap 0))
      0))

;; create-bitmap : Number Number [Number Number -> Pixel] -> ImageBitmap
;;
;; produces a bitmap of the given width and height with each element given by
;; the generator applied to the row and column. the generator should expect the
;; X and Y coordinates of the pixel in picture coordinates (i.e. (0, 0) is the
;; top-left corner).
;;
;; N.B. racket/draw already has make-bitmap which works with racket's
;; representation of bitmaps; therefore, we must use a different identifier
;; name.
(define (create-bitmap width height generator)
  (for/vector ((y (in-range height)))
    (for/vector ((x (in-range width)))
      (generator x y))))
