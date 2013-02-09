#lang racket

(require racket/cmdline)

(provide parse-arguments)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-command-line.rkt
;;
;; The command line syntax is currently:
;;
;;   spims  -p <file> -s <file>
;;
;; where the first file is a pattern image and the second file is a source image

;; parse-arguments : [Vector String] -> [Values String String]
(define (parse-arguments arguments)
  (let ((pattern-image-filename #f)
        (source-image-filename #f))
    ;; This expression returns (void) because there is no #:args clause. A #:args
    ;; clause could be used to parse arguments which succeed the regular, flagged
    ;; arguments.
    (command-line #:program "spims"
                  #:argv arguments
                  #:once-each
                  ["-p" filename "the pattern image"
                   (set! pattern-image-filename filename)]
                  ["-s" filename "the source image"
                   (set! source-image-filename filename)])
    ;; verify that the necessary arguments were passed
    (unless source-image-filename
      (error 'parse-arguments
             "You must specify a source image -- see 'spims -h' for help."))
    (unless pattern-image-filename
      (error 'parse-arguments
             "You must specify a pattern image -- see 'spims -h' for help."))
    ;; this returns two values, you can use a (let-values (((a b) ...) ...) ...)
    ;; form to capture these values
    (values pattern-image-filename source-image-filename)))
