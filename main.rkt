#lang racket

(require "data-structures.rkt"
         "load-image-file.rkt"
         "parse-command-line.rkt"
         "Algorithm.rkt"
         "print-matches.rkt")

;; match-coordinates->match : PreMatch
;;                            Number Number
;;                            String String
;;                            ->
;;                            Match
(define (pre-match->match pre-match match-width match-height
                          pattern-filename source-filename)
  (let ((x (pre-match-x pre-match))
        (y (pre-match-y pre-match))
        (avg-diff (pre-match-avg-diff pre-match)))
    (match pattern-filename source-filename
           match-width match-height
           x y avg-diff)))

;; image-filepath-pair->matches : String String Number -> [ListOf Match]
(define (image-filepath-pair->matches pattern-filename source-filename tolerance)
  (let ((pattern-image (load-image-file pattern-filename))
        (source-image (load-image-file source-filename)))

    (for/list ((pre-match
                (find-pattern-in-source/tolerance pattern-image
                                                  source-image
                                                  tolerance)))

      (pre-match->match pre-match
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
  (let-values (((pattern-filenames source-filenames debug-setting tolerance)
                (parse-arguments (current-command-line-arguments))))
    (parameterize
        ([debug debug-setting]
         ;; if we really are in debug mode, switch to a normal error reporter
         [error-display-handler (if debug
                                    regular-error-display-handler
                                    error-display-handler-no-stack-trace)])
      (for* ((pattern-filename pattern-filenames)
             (source-filename source-filenames))
        (when (debug) (printf "looking at: ~a\n" source-filename))
        (print-matches
         (image-filepath-pair->matches pattern-filename
                                       source-filename
                                       tolerance))))))
