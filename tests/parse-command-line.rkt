#lang racket

(require rackunit
         "../parse-command-line.rkt"
         "test-utils.rkt")

(provide parse-command-line-tests)

(define parse-command-line-tests
  (test-suite
   "Tests for parse-command-line.rkt"

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

    (let-values (((pattern source debug)
                  (parse-arguments (vector "-pdir" "images/A4/Sources"
                                           "-s" "images/10x30-red.jpg"))))
      (check-equal? pattern (directory-list "images/A4/Sources"))
      (check-equal? source (list (string->path "images/10x30-red.jpg")))
      (check-false debug))

    (let-values (((pattern source debug)
                  (parse-arguments (vector "-p" "images/10x30-red.jpg"
                                           "-sdir" "images/A4/Sources"))))
      (check-equal? pattern (list (string->path "images/10x30-red.jpg")))
      (check-equal? source (directory-list "images/A4/Sources"))
      (check-false debug))

    (let-values (((pattern source debug)
                  (parse-arguments (vector "-pdir" "images/A4/Patterns"
                                           "-sdir" "images/A4/Sources"))))
      (check-equal? pattern (directory-list "images/A4/Patterns"))
      (check-equal? source (directory-list "images/A4/Sources"))
      (check-false debug)))

   (test-suite
    "Assignment 4, no directories"

    (let-values (((pattern source debug)
                  (parse-arguments (vector "-p" "images/30x30-black.png" "-s" "images/10x30-red.jpg"))))
      (check-equal? pattern (list (string->path "images/30x30-black.png")))
      (check-equal? source (list (string->path "images/10x30-red.jpg")))
      (check-false debug))

    (let-values (((pattern source debug)
                  (parse-arguments (vector "-s" "images/10x30-red.jpg" "-p" "images/30x30-black.png"))))
      (check-equal? pattern (list (string->path "images/30x30-black.png")))
      (check-equal? source (list (string->path "images/10x30-red.jpg")))
      (check-false debug))

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
