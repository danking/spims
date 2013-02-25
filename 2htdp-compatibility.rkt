#lang typed/racket

(require "data-structures.rkt"
         2htdp/image)

(provide pixel->color image-bitmap/pixel->image-bitmap/color)

(: pixel->color : pixel -> Color)
(define (pixel->color p)
  (make-color (pixel-red p) (pixel-green p) (pixel-blue p)))

(: image-bitmap/pixdl->image-bitmap/color : (Vector (Vector pixel)) -> Color)
(define (image-bitmap/pixel->image-bitmap/color bm)
  (for/vector ((row (in-vector bm)))
    (for/vector ((pixel-element (in-vector row)))
      (pixel->color pixel-element))))
