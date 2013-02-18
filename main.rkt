#lang racket

(require "data-structures.rkt"
         "load-image-file.rkt"
         "parse-command-line.rkt"
         "Algorithm.rkt"
         "print-matches.rkt")

(define TOLERANCE 125)

;; match-coordinates->match : [List Number Number] Number Number String String
(define (match-coordinates->match match-coordinate match-width match-height pattern-filename source-filename)
  (let ((x (first match-coordinate))
        (y (second match-coordinate)))
    (match pattern-filename source-filename
           match-width match-height
           x y)))

;; image-filepath-pair->matches : String String -> [ListOf Match]
(define (image-filepath-pair->matches pattern-filename source-filename)
  (let ((pattern-image (load-image-file pattern-filename))
        (source-image (load-image-file source-filename)))

    (for/list ((match-coordinate (find-pattern-in-source/tolerance pattern-image
                                                                   source-image
                                                                   TOLERANCE)))

      (match-coordinates->match match-coordinate
                                (bitmap-width pattern-image)
                                (bitmap-height pattern-image)
                                pattern-filename
                                source-filename))))

(define (remove-near-duplicates list-of-matches)
  (for/fold ((keepers '()))
            ((m list-of-matches))
    (if (for/and ((keeper keepers))
          (>= (match-pixel-distance m keeper) 5))
        (cons m keepers)
        keepers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; executed section

(let-values (((pattern-filename source-filename debug-setting)
              (parse-arguments (current-command-line-arguments))))
  (parameterize ([debug debug-setting])
    (print-matches (remove-near-duplicates (image-filepath-pair->matches pattern-filename source-filename)))
    (printf "Biggest diff was ~a\n" (biggest-diff))))
