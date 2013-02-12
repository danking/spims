#lang racket

(require rackunit
         "../parse-command-line.rkt"
         "test-utils.rkt")

(provide parse-command-line-tests)

(define parse-command-line-tests
  (test-suite
   "Tests for parse-command-line.rkt"

   (let-values (((pattern source)
                 (parse-arguments (vector "-p" "foo.jpg" "-s" "bar.png"))))
     (check-equal? pattern "foo.jpg")
     (check-equal? source "bar.png"))

   (let-values (((pattern source)
                 (parse-arguments (vector "-s" "bar.png" "-p" "foo.jpg"))))
     (check-equal? pattern "foo.jpg")
     (check-equal? source "bar.png"))

   ;; missing -p
   (check-error (parse-arguments (vector "-s" "bar.png" "foo.jpg")))
   ;; missing -s
   (check-error (parse-arguments (vector "bar.png" "-p" "foo.jpg")))

   ;; missing pattern filename
   (check-error (parse-arguments (vector "-s" "bar.png" "-p")))
   ;; missing source filename
   (check-error (parse-arguments (vector "-s" "-p" "foo.jpg")))
   ;; missing both filenames
   (check-error (parse-arguments (vector "-s" "-p")))

   ;; missing source
   (check-error (parse-arguments (vector "-p" "foo.jpg")))
   ;; missing pattern
   (check-error (parse-arguments (vector "-s" "bar.png")))

   ;; no arguments
   (check-error (parse-arguments (vector)))
   ;; bogus extra flag
   (check-error (parse-arguments (vector "-s" "bar.png" "-p" "foo.jpg" "-b")))
   ;; bogus extra non-flag argument
   (check-error (parse-arguments (vector "-s" "bar.png" "-p" "foo.jpg" "bogus")))))
