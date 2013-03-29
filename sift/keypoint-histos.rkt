#lang typed/racket

(require images/flomap
         racket/flonum
         "math-utils.rkt"
         "flonum-utils.rkt"
         "histogram.rkt")

(define sample-window-width 16)
(define sample-window-width/2 8)
(define descriptor-window-width 4)
(define block-size 4)

(unless (= block-size
           (/ sample-window-width descriptor-window-width))
  (error 'keypoint-histos "check block-size"))
(unless (= sample-window-width/2
           (/ sample-window-width 2))
  (error 'keypoint-histos "check sample-window-width/2"))


(define pi/2 (/ pi 2))

;; For each keypoint, get a 16x16 grid of points around it and find their m and
;; theta. Weight the m values by a Gaussian centered at the key point with a
;; characteristic width of 8. Break the 16x16 grid into an 4x4 grid of 4x4 blocks,
;; and produce a histogram (with the x-values being thetas, and y-values being
;; magnitudes) of the vectors for each 4x4 block. Use 8 bins per histogram.

;; There should be 16 histograms of 8 bins per histogram for each keypoint. This
;; means that a keypoint is represented by 128 numbers.

(define-type keypoint-histos [Vectorof [Vectorof histogram]])

(: keypoint->histograms :
   Natural Natural
   [Integer Integer -> Flonum]
   [Integer Integer -> Flonum]
   ->
   keypoint-histos)
(define (keypoint->histograms x y m-vals theta-vals)
  (window-of-vectors->keypoint-histograms
   (keypoint->window-of-vectors x y m-vals theta-vals)))

 (: keypoint->window-of-vectors :
   Natural Natural
   [Natural Natural -> Flonum]
   [Natural Natural -> Flonum]
   ->
   [Vectorof [Vectorof [Vector Flonum Flonum]]])
(define (keypoint->window-of-vectors x y m-vals theta-vals)
  (let ((x0 (- x sample-window-width/2))
        (y0 (- x sample-window-width/2))
        (x1 (+ x (sub1 sample-window-width/2)))
        (y1 (+ x (sub1 sample-window-width/2)))
        (#{gaus : [Flonum -> Flonum]}
         (normal-distribution (->fl sample-window-width/2))))
    (let ((weighted-m-vals (lambda: ((x : Natural) (y : Natural))
                             (* (gaus (sqrt (+ (sqr (->fl x)) (sqr (->fl y)))))
                                (m-vals x y)))))
      (for/vector: : (Vectorof (Vectorof (Vector Flonum Flonum)))
                   ((y (in-range y0 y1)))
        (for/vector: : (Vectorof (Vector Flonum Flonum))
                     ((x (in-range x0 x1)))
          (vector (weighted-m-vals (max 0 x) (max 0 y))
                  (theta-vals (max 0 x) (max 0 y))))))))

(: window-of-vectors->keypoint-histograms :
   [Vectorof [Vectorof [Vector Flonum Flonum]]] -> keypoint-histos)
(define (window-of-vectors->keypoint-histograms vecs)
  (for/vector: : [Vectorof [Vectorof histogram]]
               ((y0 (in-range 0 sample-window-width block-size)))
    (for/vector: : [Vectorof histogram]
                 ((x0 (in-range 0 sample-window-width block-size)))
      (block->histogram (get-block-at vecs (max 0 x0) (max 0 y0))))))

(: get-block-at :
   (All (X)
        [Vectorof [Vectorof X]] Natural Natural
        ->
        [Natural Natural -> X]))
(define (get-block-at vecs x0 y0)
  (lambda: ((x : Natural) (y : Natural))
    (vector-ref (vector-ref vecs (+ y y0)) (+ x x0))))

(: block->histogram :
   [Natural Natural -> [Vector Flonum Flonum]]
   ->
   histogram)
(define (block->histogram block)
  (for/fold: : histogram
      ((hist (make-hist (- pi/2) pi/2 8)))
      ((x (in-range 0 block-size)))
    (for/fold: : histogram
        ((hist hist))
        ((y (in-range 0 block-size)))
      (let* ((element (block (max 0 x) (max 0 y)))
             (mag (vector-ref element 0))
             (angle (vector-ref element 1)))
        (hist-add hist angle mag)))))
