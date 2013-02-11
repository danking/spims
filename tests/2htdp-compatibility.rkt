#lang racket

(require rackunit
         "../2htdp-compatibility.rkt"
         "../data-structures.rkt"
         2htdp/image)

(provide 2htdp-compatibility-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests/2htdp-compatibility-tests

(define 10x10-black-bitmap/pixel
  (create-bitmap 10 10 (lambda (i j) (pixel 0 0 0))))
(define 10x30-red-bitmap/pixel
  (create-bitmap 10 30 (lambda (i j) (pixel 255 0 0))))

(define 10x10-black-bitmap/color
  ;; this is an abuse of the create-bitmap function, but this whole module is
  ;; a compatibility hack, so we'll run with it
  (create-bitmap 10 10 (lambda (i j) (make-color 0 0 0))))
(define 10x30-red-bitmap/color
  (create-bitmap 10 30 (lambda (i j) (make-color 255 0 0))))

(define 2htdp-compatibility-tests
  (test-suite
   "Tests for 2htdp-compatibility-tests.rkt"

   (test-suite
    "pixel->color"

    (test-equal? "10x10 black bitmap"
                 (image-bitmap/pixel->image-bitmap/color 10x10-black-bitmap/pixel)
                 10x10-black-bitmap/color)
    (test-equal? "10x30 red bitmap"
                 (image-bitmap/pixel->image-bitmap/color 10x30-red-bitmap/pixel)
                 10x30-red-bitmap/color))))
