#lang racket

(require rackunit
         "../load-image-file.rkt"
         "../data-structures.rkt"
         "test-utils.rkt")

(provide load-image-file-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests/load-iamge-file.rkt
;;
;; Note that image dimensions are given in WIDTH x HEIGHT.

(define 30x30-black-pixel-matrix
  (create-bitmap 30 30 (lambda (r c) (pixel 0 0 0))))
(define 10x30-red-pixel-matrix
  (create-bitmap 10 30 (lambda (r c) (pixel 255 0 0))))
(define 30x10-red-pixel-matrix
  (create-bitmap 30 10 (lambda (r c) (pixel 255 0 0))))

;; JPEGs seem to convert true red to slightly less than red
(define 10x30-red-pixel-matrix-jpg
  (create-bitmap 10 30 (lambda (r c) (pixel 254 0 0))))
(define 30x10-red-pixel-matrix-jpg
  (create-bitmap 30 10 (lambda (r c) (pixel 254 0 0))))


(define load-image-file-tests
  (test-suite
   "Tests for load-image-file.rkt"

   (test-suite
    "Assignment 5"

    (check-not-exn (lambda () (load-image-file "images/A5/Patterns/tree.jpg"))))

   (test-suite
    "30x30 black images"

    (test-equal? "jpg"
                 (load-image-file "images/30x30-black.jpg")
                 30x30-black-pixel-matrix)
    (test-equal? "png"
                 (load-image-file "images/30x30-black.png")
                 30x30-black-pixel-matrix)
    (test-equal? "gif"
                 (load-image-file "images/30x30-black.gif")
                 30x30-black-pixel-matrix))

   (test-suite
    "10x30 red images"

    (test-equal? "jpg"
                 (load-image-file "images/10x30-red.jpg")
                 10x30-red-pixel-matrix-jpg)
    (test-equal? "png"
                 (load-image-file "images/10x30-red.png")
                 10x30-red-pixel-matrix)
    (test-equal? "gif"
                 (load-image-file "images/10x30-red.gif")
                 10x30-red-pixel-matrix))

   (test-suite
    "30x10 red images"

    (test-equal? "jpg"
                 (load-image-file "images/30x10-red.jpg")
                 30x10-red-pixel-matrix-jpg)
    (test-equal? "png"
                 (load-image-file "images/30x10-red.png")
                 30x10-red-pixel-matrix)
    (test-equal? "gif"
                 (load-image-file "images/30x10-red.gif")
                 30x10-red-pixel-matrix))

   (test-suite
    "non-existent files"

    (check-error (load-image-file "image/bogus-nonexistent-filename")))))
