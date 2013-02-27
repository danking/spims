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

(define (error-display-handler-no-stack-trace message exn)
  (printf "spims: ~a\n" message))
(define regular-error-display-handler (error-display-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; executed section

(parameterize
    ;; assume we're in debug mode.
    ([error-display-handler error-display-handler-no-stack-trace])
  (let-values (((pattern-filenames source-filenames debug-setting)
                (parse-arguments (current-command-line-arguments))))
    (parameterize
        ([debug debug-setting]
         ;; if we really are in debug mode, switch to a normal error reporter
         [error-display-handler (if debug
                                    regular-error-display-handler
                                    error-display-handler-no-stack-trace)])
      (for* ((pattern-filename pattern-filenames)
             (source-filename source-filenames))
            (print-matches
             (image-filepath-pair->matches pattern-filename
                                           source-filename))))))
