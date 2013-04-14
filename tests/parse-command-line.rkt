#lang racket

(require rackunit
         "../parse-command-line.rkt"
         "test-utils.rkt")

(provide parse-command-line-tests)

(define default-tolerance 26)

(define parse-command-line-tests
  (test-suite
   "Tests for parse-command-line.rkt"

   (test-suite
    "algo"

    (let-values (((pattern source ll tolerance algo)
                  (parse-arguments (vector "-d" "-d" "-pdir" "images/A4/Sources"
                                           "-s" "images/10x30-red.jpg"
                                           "--algorithm" "c-style"))))
      (check-equal? algo 'c-style))
    (let-values (((pattern source ll tolerance algo)
                  (parse-arguments (vector "-d" "-d" "-pdir" "images/A4/Sources"
                                           "-s" "images/10x30-red.jpg"))))
      (check-equal? algo 'c-style))
    (let-values (((pattern source ll tolerance algo)
                  (parse-arguments (vector "-d" "-d" "-pdir" "images/A4/Sources"
                                           "-s" "images/10x30-red.jpg"
                                           "--algorithm" "brute-force"))))
      (check-equal? algo 'brute-force)))

   (test-suite
    "loglevel"

    (let-values (((pattern source ll tolerance algo)
                  (parse-arguments (vector "-d" "-d" "-pdir" "images/A4/Sources"
                                           "-s" "images/10x30-red.jpg"
                                           "--tolerance" "42"))))
      (check-equal? ll 2))
    (let-values (((pattern source ll tolerance algo)
                  (parse-arguments (vector "-d" "-pdir" "images/A4/Sources"
                                           "-s" "images/10x30-red.jpg"
                                           "--tolerance" "42"))))
      (check-equal? ll 1))
    (let-values (((pattern source ll tolerance algo)
                  (parse-arguments (vector "-pdir" "images/A4/Sources"
                                           "-s" "images/10x30-red.jpg"
                                           "--tolerance" "42"))))
      (check-equal? ll 0)))

   (test-suite
    "Assignment 5, directories"

    (test-suite
     "path is not a directory"
     (check-error (parse-arguments (vector "-pdir" "images/10x30-red.jpg" "-s" "images/10x30-red.jpg")))
     (check-error (parse-arguments (vector "-sdir" "images/10x30-red.jpg" "-p" "images/10x30-red.jpg"))))

    (test-suite
     "directory doesn't exist"
     (check-error (parse-arguments (vector "-pdir" "DOESNTEXISTDIR/" "-s" "images/10x30-red.jpg")))
     (check-error (parse-arguments (vector "-sdir" "DOESNTEXISTDIR/" "-p" "images/10x30-red.jpg"))))

    (let-values (((pattern source ll tolerance algo)
                  (parse-arguments (vector "-pdir" "images/A4/Sources"
                                           "-s" "images/10x30-red.jpg"
                                           "--tolerance" "42"))))
      (check-equal? pattern (directory-list "images/A4/Sources" #:build? #t))
      (check-equal? source (list (string->path "images/10x30-red.jpg")))
      (check-equal? ll 0)
      (check-equal? tolerance 42)
      (check-equal? algo 'c-style))

    (let-values (((pattern source ll tolerance algo)
                  (parse-arguments (vector "-d" "-pdir" "images/A4/Sources"
                                           "-s" "images/10x30-red.jpg"))))
      (check-equal? pattern (directory-list "images/A4/Sources" #:build? #t))
      (check-equal? source (list (string->path "images/10x30-red.jpg")))
      (check-equal? ll 1)
      (check-equal? tolerance default-tolerance)
      (check-equal? algo 'c-style))

    (let-values (((pattern source ll tolerance algo)
                  (parse-arguments (vector "-pdir" "images/A4/Sources"
                                           "-s" "images/10x30-red.jpg"))))
      (check-equal? pattern (directory-list "images/A4/Sources" #:build? #t))
      (check-equal? source (list (string->path "images/10x30-red.jpg")))
      (check-equal? ll 0)
      (check-equal? tolerance default-tolerance)
      (check-equal? algo 'c-style))

    (let-values (((pattern source ll tolerance algo)
                  (parse-arguments (vector "-p" "images/10x30-red.jpg"
                                           "-sdir" "images/A4/Sources"))))
      (check-equal? pattern (list (string->path "images/10x30-red.jpg")))
      (check-equal? source (directory-list "images/A4/Sources" #:build? #t))
      (check-equal? ll 0)
      (check-equal? tolerance default-tolerance)
      (check-equal? algo 'c-style))

    (let-values (((pattern source ll tolerance algo)
                  (parse-arguments (vector "-pdir" "images/A4/Patterns"
                                           "-sdir" "images/A4/Sources"))))
      (check-equal? pattern (directory-list "images/A4/Patterns" #:build? #t))
      (check-equal? source (directory-list "images/A4/Sources" #:build? #t))
      (check-equal? ll 0)
      (check-equal? tolerance default-tolerance)
      (check-equal? algo 'c-style)))

   (test-suite
    "Assignment 4, no directories"

    (let-values (((pattern source ll tolerance algo)
                  (parse-arguments (vector "-p" "images/30x30-black.png" "-s" "images/10x30-red.jpg"))))
      (check-equal? pattern (list (string->path "images/30x30-black.png")))
      (check-equal? source (list (string->path "images/10x30-red.jpg")))
      (check-equal? ll 0)
      (check-equal? tolerance default-tolerance)
      (check-equal? algo 'c-style))

    (let-values (((pattern source ll tolerance algo)
                  (parse-arguments (vector "-s" "images/10x30-red.jpg" "-p" "images/30x30-black.png"))))
      (check-equal? pattern (list (string->path "images/30x30-black.png")))
      (check-equal? source (list (string->path "images/10x30-red.jpg")))
      (check-equal? ll 0)
      (check-equal? tolerance default-tolerance)
      (check-equal? algo 'c-style))

    ;; missing -p
    (check-error (parse-arguments (vector "-s" "images/10x30-red.jpg" "images/30x30-black.png")))
    ;; missing -s
    (check-error (parse-arguments (vector "images/10x30-red.jpg" "-p" "images/30x30-black.png")))

    ;; missing pattern filename
    (check-error (parse-arguments (vector "-s" "images/10x30-red.jpg" "-p")))
    ;; missing source filename
    (check-error (parse-arguments (vector "-s" "-p" "images/30x30-black.png")))
    ;; missing both filenames
    (check-error (parse-arguments (vector "-s" "-p")))

    ;; missing source
    (check-error (parse-arguments (vector "-p" "images/30x30-black.png")))
    ;; missing pattern
    (check-error (parse-arguments (vector "-s" "images/10x30-red.jpg")))

    ;; no arguments
    (check-error (parse-arguments (vector)))
    ;; bogus extra flag
    (check-error (parse-arguments (vector "-s" "images/10x30-red.jpg" "-p" "images/30x30-black.png" "-b")))
    ;; bogus extra non-flag argument
    (check-error (parse-arguments (vector "-s" "images/10x30-red.jpg" "-p" "images/30x30-black.png" "bogus"))))))
