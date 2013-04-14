#lang racket

(require "data-structures.rkt"
         "load-image-file.rkt"
         "parse-command-line.rkt"
         "algorithms.rkt"
         "print-matches.rkt"
         "logging.rkt")

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
(define (image-filepath-pair->matches pattern-filename source-filename tolerance
                                      algo)
  (cond [(eq? algo 'c-style) (do-c-style pattern-filename source-filename)]
        [else (do-brute-force pattern-filename source-filename tolerance)]))

(define (do-c-style pattern-filename source-filename)
  (let ((pattern-image (filename->bitmap% pattern-filename))
        (source-image (filename->bitmap% source-filename)))
    (for/list ((pre-match (c-style pattern-image source-image)))
      (pre-match->match pre-match
                        (send pattern-image get-width)
                        (send pattern-image get-height)
                        pattern-filename
                        source-filename))))

(define (do-brute-force pattern-filename source-filename tolerance)
  (let ((pattern-image (load-image-file pattern-filename))
        (source-image (load-image-file source-filename)))
   (for/list
       ((pre-match (brute-force/tolerance pattern-image
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
  (let-values (((pattern-filenames source-filenames log-level tolerance algo)
                (parse-arguments (current-command-line-arguments))))
    (set-log-level log-level)
    (parameterize
        (;; if we really are in debug mode, switch to a normal error reporter
         [error-display-handler (if (debug)
                                    regular-error-display-handler
                                    error-display-handler-no-stack-trace)])
      (for* ((pattern-filename pattern-filenames)
             (source-filename source-filenames))

        (debug-msg "looking at: ~a\n" source-filename)

        (print-matches (image-filepath-pair->matches pattern-filename
                                                     source-filename
                                                     tolerance
                                                     algo))))))
