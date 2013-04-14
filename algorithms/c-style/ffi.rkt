#lang racket

(provide generate-sad-values)

(require ffi/unsafe
         ffi/unsafe/define
         "../../data-structures.rkt"
         "sad-data.rkt"
         racket/runtime-path)

(define-runtime-path libsad-path "./c/libsad")
(define-ffi-definer define-sad (ffi-lib libsad-path))

(define-sad from_racket
  (_fun (pattern : _pointer)
        (source : _pointer)
        (pattern_w : _int)
        (pattern_h : _int)
        (source_w : _int)
        (source_h : _int)
        ->
        (sad_array : _pointer)))

(define-sad sad_at
  (_fun (pattern : _pointer)
        (source : _pointer)
        (tlx : _int)
        (tly : _int)
        (pattern_w : _int)
        (pattern_h : _int)
        (source_w : _int)
        ->
        (result : _int)))

(define-cstruct _prematch ([val _int]
                           [x _int]
                           [y _int]))

(define-sad get_max
  (_fun -> (result : _prematch)))

(define-sad get_min
  (_fun -> (result : _prematch)))

(define (bitmap->bytes bm)
  (let ((w (send bm get-width))
        (h (send bm get-height)))
    (let ((bs (make-bytes (* w h 4))))
      (send bm get-argb-pixels
            0 0 w h
            bs)
      bs)))

(define (c-prematch->prematch v)
  (pre-match (prematch-x v)
             (prematch-y v)
             (prematch-val v)))

(define (generate-sad-values pat src)
  (let ((patw (send pat get-width))
        (path (send pat get-height))
        (srcw (send src get-width))
        (srch (send src get-height)))
    (let ((sadw (add1 (- srcw patw)))
          (sadh (add1 (- srch path))))
      (sad
       (make-sized-byte-string
        (from_racket (bitmap->bytes pat)
                     (bitmap->bytes src)
                     patw
                     path
                     srcw
                     srch)
        (* (add1 (- srcw patw))
           4 ;; because ints are 4 bytes
           (add1 (- srch path))))
       sadw sadh
       (stats (c-prematch->prematch (get_max))
              (c-prematch->prematch (get_min))
              #f
              #f)))))


