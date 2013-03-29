#lang racket

(require racket/draw)
(require images/flomap)

(provide Pyramid)

;; globals for k and sigma
(define k (sqrt 2))
(define sigma 1.6)


;; This will be deleted later, we will just use load-image-file
;; Create a bitmap
(define BM (make-object bitmap% 1 1 #f #f))
;; load the file
(send BM load-file "test.png")

(define FM (bitmap->flomap BM))

;; scales flomaps to half their size
(define (scale-to-half FM)
  (flomap-scale FM .5 .5))

;; performs gaussian blur on a flomap
(define (generate-interval FM k-power)
  (define blur-intensity (* sigma (expt k k-power)))
  (flomap-gaussian-blur FM blur-intensity blur-intensity))

;; generates an octave of difference of gaussians
(define (generate-octave FM)
  (vector  (fm- (generate-interval FM 1)
                (generate-interval FM 0))
           (fm- (generate-interval FM 2)
                (generate-interval FM 1))
           (fm- (generate-interval FM 3)
                (generate-interval FM 2))))

;; setting up the scaled image versions for the octaves
(define Octave-0-FM FM)
(define Octave-1-FM (scale-to-half Octave-0-FM))
(define Octave-2-FM (scale-to-half Octave-1-FM))
(define Octave-3-FM (scale-to-half Octave-2-FM))
(define Octave-4-FM (scale-to-half Octave-3-FM))

;; Generate the laplacian gaussians of these octaves
(define Octave-0 (generate-octave Octave-0-FM))
(define Octave-1 (generate-octave Octave-1-FM))
(define Octave-2 (generate-octave Octave-2-FM))
(define Octave-3 (generate-octave Octave-3-FM))
(define Octave-4 (generate-octave Octave-4-FM))

;; put the octaves in order in a Pyramid vector.
(define Pyramid
  (vector Octave-0 Octave-1 Octave-2 Octave-3 Octave-4))