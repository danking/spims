#lang racket

(define BORDER-WIDTH 5)

(define (find-extrema octaves num-octaves num-intervals )
  (define extrema '())
  (define current-octave 0)
  (define current-interval 0)
  (for/vector ([octave octaves])
    (for/vector ([interval octave])
       ;; Some code for this interval
       (define current-interval (add1 current-interval)))
     (define current-octave (add1 current-octave))
     (define current-interval 0)))






;; Determines whether a pixel is a scale-space extremum by comparing it to it's
;; 3x3x3 pixel neighborhood.

;; dog_pyr DoG scale space pyramid
;; octv pixel's scale space octave
;; intvl pixel's within-octave interval
;; r pixel's image row
;; c pixel's image col
;;
;; Returns #true if the specified pixel is an extremum (max or min) among
;; it's 3x3x3 pixel neighborhood.
(define (is_extremum dog-pyr octv intvl r c)
  {define pixel-val (pixel-from-flowmap dog-pyr octv intvl r c)}
  (if (> pixel-val 0)                                  ;; This logic is from the C code, I'm not sure if it's right for us
      (check-for-max dog-pyr octv intvl r c pixel-val) ;; If stuff isn't working, consider that this may be at fault. I'm sorry!
      (check-for-min dog-pyr octv intvl r c pixel-val)))

;; Accesses a single pixel in the Gaussian Pyramid
(define (pixel-from-dog dog-pyr octv intvl r c)
  (flomap-ref* (vector-ref (vector-ref dog-pyr octv) intvl) c r))

;; Is this a max?
(define (check-for-max dog-pyr octv intvl r c p-val)
  (for ([i '(-1 0 1)])
     (for ([j '(-1 0 1)])
         (for ([k '(-1 0 1)])
             (>= p-val (pixel-from-flomap dog-pyr octv (+ i intvl) (+ j r) (+ k c)))))))

;; Is this a min?
(define (check-for-min dog-pyr octv intvl r c p-val)
  (for ([i '(-1 0 1)])
     (for ([j '(-1 0 1)])
         (for ([k '(-1 0 1)])
             (=< p-val (pixel-from-flomap dog-pyr octv (+ i intvl) (+ j r) (+ k c)))))))
