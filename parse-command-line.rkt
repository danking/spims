#lang racket

(require racket/cmdline)

(provide parse-arguments)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-command-line.rkt
;;
;; The command line syntax is currently:
;;
;;   spims (-p <file> | -pdir <folder>) (-s <file> | -sdir <folder>)
;;
;; e.g.
;;
;;   spims -p file.jpg -sdir sources/
;;
;; where the first file (or folder) is a pattern image (or images) and the
;; second file (or folder) is a source image (or images).

;; parse-arguments : [Vector String] -> [Values [Listof Path] [Listof Path]]
(define (parse-arguments arguments)
  (let ((pattern-image-filenames '())
        (source-image-filenames '())
        (debug #f)
        (tolerance 26))
    ;; This expression returns (void) because there is no #:args clause. A #:args
    ;; clause could be used to parse arguments which succeed the regular, flagged
    ;; arguments.
    (command-line #:program "spims"
                  ;; fix-args is a hack for single-dash args, see below ...
                  #:argv (fix-args arguments)
                  ;; patterns
                  #:once-any
                  ["-p" filename "the pattern image"
                   (unless (file-exists? filename)
                     (error 'parse-arguments
                            (format "The file ~a does not exist!" filename)))
                   (set! pattern-image-filenames (list (string->path filename)))]
                  ["--pdir" foldername "the pattern folder"
                   (unless (directory-exists? foldername)
                     (error 'parse-arguments
                            (format "The directory ~a does not exist (or isn't a directory)!"
                                    foldername)))
                   (set! pattern-image-filenames (directory-list foldername #:build? #t))]
                  ;; sources
                  #:once-any
                  ["-s" filename "the source image"
                   (unless (file-exists? filename)
                     (error 'parse-arguments
                            (format "the file ~a does not exist!" filename)))
                   (set! source-image-filenames (list (string->path filename)))]
                  ["--sdir" foldername "the source folder"
                   (unless (directory-exists? foldername)
                     (error 'parse-arguments
                            (format "The directory ~a does not exist (or isn't a directory)!"
                                    foldername)))
                   (set! source-image-filenames (directory-list foldername #:build? #t))]
                  ;; debug flag
                  #:once-each
                  [("-d" "--debug") "increased debug output"
                   (set! debug #t)]
                  ["--tolerance" t "the average pixel difference tolerance"
                   (set! tolerance (string->number t))])
    ;; verify that the necessary arguments were passed
    (when (empty? source-image-filenames)
      (error 'parse-arguments
             "You must specify at least one source image (is the directory empty?) -- see 'spims -h' for help."))
    (when (empty? pattern-image-filenames)
      (error 'parse-arguments
             "You must specify at least one pattern image (is the directory empty?) -- see 'spims -h' for help."))
    ;; this returns two values, you can use a (let-values (((a b) ...) ...) ...)
    ;; form to capture these values
    (values pattern-image-filenames source-image-filenames debug tolerance)))


;; fix-args : [Vector String] -> [Vector String]
;;
;; For reasons beyond my comprehension, the implementer of the `command-line'
;; syntax didn't think that anyone would use flags that have one dash and more
;; than one subsequent character. Wonderful.
;;
;; This little function replaces our expected single dash, multiple character
;; flags with the double-dash versions that `command-line' expects.
(define (fix-args argv)
  (for/vector ((arg argv))
    (cond [(equal? arg "-pdir") "--pdir"]
          [(equal? arg "-sdir") "--sdir"]
          [else arg])))
