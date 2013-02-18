#lang racket

(require rackunit
         "../Algorithm.rkt"
         "../data-structures.rkt")

(provide algorithm-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests/load-iamge-file.rkt

;; A bunch of color and vector building blocks for testing

(define blue (pixel 0 0 255))
(define red (pixel 255 0 0))
(define green (pixel 0 255 0))

(define blue-vector (vector blue blue blue blue blue))
(define green-vector (vector green green green green green))
(define red-vector (vector red red red red red))

(define 20-red-vector (vector-append red-vector red-vector red-vector red-vector))
(define 20-green-vector (vector-append green-vector green-vector green-vector green-vector))
(define 20-blue-vector (vector-append blue-vector blue-vector blue-vector blue-vector))

(define 1x1red
  (vector (vector red)))

(define 1x1green
  (vector (vector green)))

(define 1x1blue
  (vector (vector blue)))

(define 5x5red
  (vector red-vector red-vector red-vector red-vector red-vector))

(define 5x5blue
  (vector blue-vector blue-vector blue-vector blue-vector blue-vector))

(define 5x5green
  (vector green-vector green-vector green-vector green-vector green-vector))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing procedures


;; check-time<= should be passed three arguments, the first is the name of the
;; procedure we would like to time, the second is a *list* of arguments for said
;; procedure, the third is the expected result of the procedure, and the fourth
;; is the time maximum duration for which the procedure should run.
(define-check (check-time<= name procedure argument-list expected time)
  (let-values (((results cpu-time real-time gc-time)
                (time-apply procedure argument-list)))
    (unless (equal? (first results) expected)
      (with-check-info*
       (list (make-check-name name)
             (make-check-actual (first results))
             (make-check-expected expected)
             (make-check-message
              (format "Values didn't match; cpu-time: ~a, real-time: ~a, gc-time ~a."
                      cpu-time real-time gc-time)))
       (lambda () (fail-check))))
    (unless (<= real-time time)
      (with-check-info*
       (list (make-check-name name)
             (make-check-message
              (format (string-append "Values matched, but time was over limit of "
                                     "~a; cpu-time: ~a, real-time: ~a, gc-time ~a.")
                      time cpu-time real-time gc-time)))
       (lambda () (fail-check))))))

(define algorithm-tests
  (test-suite
   "Tests for Algorithm.rkt"

   ;; Some tests

   ;; Currently working on the vector-ref error... :/

   (test-suite
    "timing tests"

    (let ((max-running-time 10000))
      (check-time<= "5x5 green with 5x5 blue"
        find-pattern-in-source (list 5x5green 5x5blue) '() max-running-time)
      (check-time<= "5x5 blue with 5x5 red"
        find-pattern-in-source (list 5x5blue 5x5red) '() max-running-time)
      (check-time<= "5x5 red with 1x1 blue"
        find-pattern-in-source (list 5x5red 1x1blue) '() max-running-time)
      (check-time<= "1x1 red with 5x5 red"
        find-pattern-in-source (list 1x1red 5x5red)
        (for*/list ((x (in-range 0 5))
                    (y (in-range 0 5)))
          (list x y))
        max-running-time)
      (check-time<= "5x5 green with 5x5 green"
        find-pattern-in-source (list 5x5green 5x5green) '((0 0)) max-running-time)
      (check-time<= "1x1 blue with 1x1 blue"
        find-pattern-in-source (list 1x1blue 1x1blue) '((0 0)) max-running-time)))))
