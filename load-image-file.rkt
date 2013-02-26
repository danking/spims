#lang racket

(require racket/draw
         "data-structures.rkt")

(provide load-image-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-image-file.rkt
;;
;; The project must support three image formats: JPG, PNG, and GIF
;; (sans-animation). We rely on the racket image library to perform the image
;; loading.


;; load-image-file : Path -> ImageBitmap
(define (load-image-file filename)
  (let ((bitmap-object (filename->bitmap% filename)))
    (if bitmap-object
        (let-values (((image-bytes width height)
                      (bitmap%->bytes bitmap-object)))
          (bytes->pixel-matrix image-bytes width height))
        (error 'load-image-file
               (format "The file ~a does not contain an image in a valid format!"
                       filename)))))

;; filename->bitmap% : Path -> (U False bitmap%)
;;
;; loads the file into a bitmap object, or, if that fails, returns false
(define (filename->bitmap% filename)
  (let* ((new-image
          (make-object bitmap%
                       ;; the width and height don't matter because they will be
                       ;; replaced by the dimensions of the image we load
                       1 1
                       ;; we are not loading a monochrome image
                       #f
                       ;; ignore alpha channel (transparency)
                       #f))
         (load-succesful?
          (send new-image load-file
                filename
                ;; we don't know the type of file, but definitely ignore the
                ;; alpha channel (transparency)
                'unknown)))
    (if load-succesful?
        new-image
        #f)))

;; bitmap%->bytes : bitmap% -> [Values Bytes Number Number]
;;
;; extracts the entire image as a sequence of bytes and returns those bytes as
;; well as the image width and height IN PIXELS (i.e. the byte matrix is (*
;; width 4) bytes wide).
(define (bitmap%->bytes bm)
  (let* ((width (send bm get-width))
         (height (send bm get-height))
         ;; unfortunately, there is no procedure to only get the rgb values, so
         ;; we need four bytes per pixel; we will later drop the alpha value
         (number-of-bytes-necessary (* width
                                       height
                                       4))
         (byte-array-of-pixels (make-bytes number-of-bytes-necessary)))
   (send bm get-argb-pixels
         ;; get everything from the top-left corner ...
         0 0
         ;; ... to the bottom right corner ...
         width height
         ;; ... and store it in here.
         byte-array-of-pixels)
   (values byte-array-of-pixels width height)))

;; get-rgb-at : Bytes Number Number Number -> [Values Number Number Number]
;;
;; Gets an argb pixel from a byte string produced by the bitmap% object's
;; `get-argb-pixels' method.
;;
;; Note that the Bytes array is always 4 * image-width * image-height bytes
;; long, i.e., there are four bytes assigned to each pixel. The pixel channels
;; are stored in the order: Alpha, Red, Green, Blue.
(define (get-rgb-at bytes row-width-in-pixels row column)
  (let ((row-width-in-bytes (* row-width-in-pixels 4)))
    (let ((row-pixel-offset (* row row-width-in-bytes))
          (column-pixel-offset (* column 4)))
      (let ((total-offset (+ row-pixel-offset column-pixel-offset)))
        ;; note that we skip the first pixel because that's the alpha value
        (values (bytes-ref bytes (+ total-offset 1))
                (bytes-ref bytes (+ total-offset 2))
                (bytes-ref bytes (+ total-offset 3)))))))

;; bytes->pixel-matrix : Bytes Number Number
;;
;; Converts a byte-array into a vector of vector of pixel structures. Note that
;; the width and height here are the image's pixel-width and pixel-height.
(define (bytes->pixel-matrix bytes width height)
  (for/vector #:length height
              ([row (in-range height)])
    (for/vector #:length width
                ([column (in-range width)])
      (let-values (((red green blue) (get-rgb-at bytes width row column)))
        (pixel red green blue)))))
