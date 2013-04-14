#lang racket

(provide set-log-level
         debug
         debug2
         debug-msg
         debug2-msg)

(define log-level 0)

(define (set-log-level l)
  (set! log-level l))

(define (debug) (> log-level 0))
(define (debug2) (> log-level 1))

(define (debug-msg . args)
  (when (> log-level 0) (apply printf args)))

(define (debug2-msg . args)
  (when (> log-level 1) (apply printf args)))
