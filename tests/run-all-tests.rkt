#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-all-tests.rkt
;;
;; To add a new test-suite,
;;
;;  - add a file which `provide's a test suite containing various `test-cases'
;;    or `check' forms.
;;
;;  - require the new test file in this file
;;
;;  - add the `provide'd test-suite to the test-suite in the run-tests form

(require rackunit
         rackunit/text-ui ;; for run-tests
         "parse-command-line.rkt"
         "load-image-file.rkt")

(run-tests (test-suite "all-tests"
                       parse-command-line-tests
                       load-image-file-tests))



