#lang racket

(provide (all-defined-out))

(require ffi/unsafe
         ffi/unsafe/define
         "data-structures.rkt")

;; on systems machines use this:
;;(define-ffi-definer define-fftw (ffi-lib "libfftw3.so.3"))
(define-ffi-definer define-sad (ffi-lib "./libsad"))

(define-cpointer-type _int_array)

;; produces FFTW plans
(define-sad from_racket
  (_fun (pattern : _pointer) ;; fftw_complex*
        (source : _pointer) ;; fftw_complex*
        (pattern_w : _int)
        (pattern_h : _int)
        (source_w : _int)
        (source_h : _int)
        ->
        (sad_array : _pointer)))

(struct sad-array (bs w h) #:transparent)

(define (get-sad pat src)
  (let ((patw (send pat get-width))
        (path (send pat get-height))
        (srcw (send src get-width))
        (srch (send src get-height))
        (sadw (add1 (- srcw patw)))
        (sadh (add1 (- srch path))))
    (let* ((pat-bytes (make-bytes (* patw path 4)))
           (src-bytes (make-bytes (* srcw srch 4))))
      (send pat get-argb-pixels
            0 0 (sub1 patw) (sub1 path)
            pat-bytes)
      (send src get-argb-pixels
            0 0 (sub1 srcw) (sub1 srch)
            src-bytes)
      (sad-array
       (make-sized-byte-string
        (from_racket pat-bytes
                     src-bytes
                     (* patw 4)
                     path
                     (* srcw 4)
                     srch)
        (* (add1 (- srcw patw))
           4
           (add1 (- srch path))))
       sadw sadh))))

(define (find-less-than sad tolerance)
  (for*/list ((x (in-range 0 (sad-array-w sad)))
              (y (in-range 0 (sad-array-h sad)))
              #:when (< (sad-get sad x y)
                        tolerance))
    (pre-match x y (sad-get sad x y))))

(define (sad-get s x y)
  (let ((bs (sad-array-bs s))
        (w (sad-array-w s)))
   (+ (bytes-ref bs (+ x (* y w 4) 0))
      (arithmetic-shift (bytes-ref bs (+ x (* y w 4) 1)) 8)
      (arithmetic-shift (bytes-ref bs (+ x (* y w 4) 2)) 16)
      (arithmetic-shift (bytes-ref bs (+ x (* y w 4) 3)) 24))))

(define (sad-get/bytes s x y)
  (let ((bs (sad-array-bs s))
        (w (sad-array-w s)))
    (list (bytes-ref bs (+ x (* y w 4) 0))
          (bytes-ref bs (+ x (* y w 4) 1))
          (bytes-ref bs (+ x (* y w 4) 2))
          (bytes-ref bs (+ x (* y w 4) 3)))))

(define (get-sad/raw pat-bytes src-bytes patw path srcw srch)
  (make-sized-byte-string
   (from_racket pat-bytes
                src-bytes
                (* patw 4)
                path
                (* srcw 4)
                srch)
   (* (add1 (- srcw patw))
      4
      (add1 (- srch path)))))

(require "data-structures.rkt")
(define (find-less-than sad-vals tolerance)
  (for*/list ((x (in-range 0 (sad-array-w sad-vals)))
              (y (in-range 0 (sad-array-h sad-vals)))
              #:when (< (sad-get sad-vals x y)
                        tolerance))
    (pre-match x y (sad-get sad-vals x y))))

(define (image-pair->pre-matches i1 i2)
  (get-sad i1 i2))
