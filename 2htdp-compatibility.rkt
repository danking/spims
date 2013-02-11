#lang racket

(require "data-structures.rkt"
         2htdp/image)

(provide pixel->color image-bitmap/pixel->image-bitmap/color)

(define (pixel->color p)
  (make-color (pixel-red p) (pixel-green p) (pixel-blue p)))

(define (image-bitmap/pixel->image-bitmap/color bm)
  (for/vector ((row (in-vector bm)))
    (for/vector ((pixel-element (in-vector row)))
      (pixel->color pixel-element))))
