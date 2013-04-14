#lang racket

(provide sad->image sad->image->file)

(require racket/draw)

(define (scale-for-graying val min max)
  (let* ((range (- max min))
         (normalized (/ (- val min) range)))
    ;; normalized is in [0, 1]
    (exact-floor (* (sqr (sqr normalized)) 255))))

(define (bytes->image bs w h min max)
  (let ((out (make-bytes (* w h 4))))
    (for ((i (in-range 0 (* w h 4) 4)))
      (let ((gray (scale-for-graying
                   (+ (bytes-ref bs i)
                      (arithmetic-shift (bytes-ref bs (+ i 1)) 8)
                      (arithmetic-shift (bytes-ref bs (+ i 2)) 16)
                      (arithmetic-shift (bytes-ref bs (+ i 3)) 24))
                   min max)))
       (bytes-set! out i 255)
       (bytes-set! out (+ i 1) gray)
       (bytes-set! out (+ i 2) gray)
       (bytes-set! out (+ i 3) gray)))

    (let ((bm (make-object bitmap% w h)))
      (send bm set-argb-pixels 0 0 (sub1 w) (sub1 h) out)
      bm)))

;; sad->image : SAD -> Bitmap%
;;
;; produces a bitmap image representing the sad array,
;; 255 = white = lowest sad value = best match
(define (sad->image sad)
  (bytes->image (sad-bs sad)
                (sad-w sad)
                (sad-h sad)
                (sad-min sad)
                (sad-max max)))

;; sad->image->file : SAD Filename ->
;;
;; saves the sad as a png in the given file
(define (sad->image->file sad fn)
  (send (sad->image sad) save-file fn 'png))

