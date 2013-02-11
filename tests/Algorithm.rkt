#lang racket

(require rackunit
         "../Algorithm.rkt"
         picturing-programs
         "test-utils.rkt")

(provide algorithm-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests/load-iamge-file.rkt

;; A bunch of color and vector building blocks for testing

(define blue (make-color 0 0 255))
(define red (make-color 255 0 0))
(define green (make-color 0 255 0))

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
(define-check (check-time<= procedure argument-list expected time)
  (let-values (((results cpu-time real-time gc-time)
                (time-apply procedure argument-list)))
    (unless (and (equal? (first results) expected)
                 (<= real-time time))
      (fail-check))))

(define algorithm-tests
  (test-suite
   "Tests for Algorithm.rkt"

   ;; Some tests

   ;; Currently working on the vector-ref error... :/

   (test-suite
    "timing tests"

    (let ((max-running-time 10000))
      (check-time<= match (list 5x5green 5x5blue) false max-running-time)
      (check-time<= match (list 5x5blue 5x5red) false max-running-time)
      (check-time<= match (list 5x5red 1x1blue) false max-running-time)
      (check-time<= match (list 1x1red 5x5red) false max-running-time)
      (check-time<= match (list 5x5green 5x5green) false max-running-time)
      (check-time<= match (list 1x1blue 1x1blue) true max-running-time)))))
