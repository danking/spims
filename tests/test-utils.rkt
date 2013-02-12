#lang racket

(require rackunit)

(provide check-error)

(define-syntax check-error
  (syntax-rules ()
    ((check-error expression)
     (check-exn exn:fail? (lambda () expression)))))
