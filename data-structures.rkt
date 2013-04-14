#lang racket

(provide (struct-out pixel) (struct-out match) (struct-out pre-match)
         get-pixel-at bitmap-width bitmap-height
         create-bitmap pixels-match-with-tolerance?
         pixel-difference-in-bitmap-at get-diff-sum
         match-pixel-distance
         ;; parameters
         debug biggest-diff)

(require "logging.rkt")

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

(define biggest-diff (make-parameter 0))

;; pixels-match-with-tolerance? ImageBitmap ImageBitmap Number Number Number Number -> Boolean
;;
;; Compares the tolerance global to the sum of the differences of P and T's pixel color components
(define (pixels-match-with-tolerance? p t px py tx ty tolerance)
  (let* ([difference (pixel-difference-in-bitmap-at p t px py tx ty)]
         [tolerable? (<= difference tolerance)])
    (when (and (debug) tolerable?)
      (biggest-diff difference))
    tolerable?))

(define (pixel-difference-in-bitmap-at pattern source
                                       pattern-x pattern-y
                                       source-x source-y)
  (let* ([p-pixel (get-pixel-at pattern pattern-y pattern-x)]
         [t-pixel (get-pixel-at source source-y source-x)])
    (get-diff-sum p-pixel t-pixel)))

;; get-diff-sum pixel pixel -> Number
;;
;; Computes the Sum of absolute difference value for two pixels
(define (get-diff-sum pixel1 pixel2)
  (+ (abs (- (pixel-red pixel1) (pixel-red pixel2)))
     (abs (- (pixel-green pixel1) (pixel-green pixel2)))
     (abs (- (pixel-blue pixel1) (pixel-blue pixel2)))))

;; a Match is a
;;   (match String String Number Number Number Number Number)
;; where the fields:
;;
;;  - pattern-img is the pattern image's filename
;;
;;  - source-img is the source image's filename
;;
;;  - m1 is the width of the subimage that matches (which should be the same as
;;    the width of the pattern image)
;;
;;  - n1 is the height of the subimage that matches <file1> (which should be the
;;    same as the height of the pattern image)
;;
;;  - x is the horizontal offset of the top left corner of that subimage from
;;    the top left corner of the source image
;;
;;  - y is the vertical offset of the top left corner of that subimage from the
;;    top left corner of the source image
;;
;;  - avg-diff is the average pixel difference in this match
;;
(struct match (pattern-img source-img m1 n1 x y avg-diff))

;; a pre-match is a match struct with some missing information
(struct pre-match (x y avg-diff) #:transparent)

(define (match-pixel-distance m1 m2)
  (sqrt (+ (sqr (- (match-x m1) (match-x m2)))
           (sqr (- (match-y m1) (match-y m2))))))
