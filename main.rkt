#lang racket

(require "Algorithm.rkt"
         "load-image-file.rkt"
         "parse-command-line.rkt")

(let-values (((pattern-filename source-filename)
              (parse-arguments (current-command-line-arguments))))
  (let ((pattern-image (load-image-file pattern-filename))
        (source-image (load-image-file source-filename)))
    (match pattern-image source-image pattern-filename source-filename)))
