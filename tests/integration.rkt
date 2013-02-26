#lang racket

(require rackunit)

(provide integration-tests)

;; run-spims : String -> [Set String]
(define (run-spims invocation)
  (match-let (((list stdout stdin pid stderr communicate-with-process)
               (process invocation)))
      ;; wait until the process completes
    (communicate-with-process 'wait)
    (close-output-port stdin)
    (let ((match-strings-set (list->set (port->lines stdout)))
          (errors (port->string stderr)))
      (close-input-port stdout)
      (close-input-port stderr)
      (values match-strings-set errors))))

(define-check (spims-matches-check invocation expected)
  (displayln invocation)
  (let-values (((actual errors) (run-spims invocation)))
    (unless (equal? actual expected)
      (display "stderr for the following check was:\n")
      (displayln errors)
      (with-check-info*
       (list (make-check-message
              (string-append
               "Actual output did not exactly match expected output. Actual "
               "lists those results which appear in the actual results but "
               "were not expected.  Expected lists those matches that were "
               "expected, but not found in actual output."))
             (make-check-actual (set-subtract actual expected))
             (make-check-expected (set-subtract expected actual)))
       (lambda () (fail-check))))))

(define integration-tests
  (test-suite
   "integration test"

   (unless (file-exists? "../spims")
     (with-check-info*
      (list (make-check-message "executable file '../spims' does not exist!"))
      (lambda () (fail-check))))

   (spims-matches-check "../spims -pdir images/A5/Patterns -sdir images/A5/Sources"
                        (set "green23.png matches greenish.png at 2x3+0+0"
                             "green23.png matches greenish.png at 2x3+0+6"
                             "green23.png matches greenish.png at 2x3+1+1"
                             "green23.png matches greenish.png at 2x3+1+7"
                             "green23.png matches greenish.png at 2x3+2+2"
                             "green23.png matches greenish.png at 2x3+4+7"
                             "green23.png matches greenish.png at 2x3+5+0"
                             "green23.png matches greenish.png at 2x3+7+2"
                             "green23.png matches greenish.png at 2x3+8+0"
                             "green23.png matches greenish.png at 2x3+8+4"
                             "red1.png matches greenish.png at 1x1+0+3"
                             "red1.png matches greenish.png at 1x1+0+5"
                             "red1.png matches greenish.png at 1x1+0+9"
                             "red1.png matches greenish.png at 1x1+1+4"
                             "red1.png matches greenish.png at 1x1+2+0"
                             "red1.png matches greenish.png at 1x1+2+6"
                             "red1.png matches greenish.png at 1x1+3+1"
                             "red1.png matches greenish.png at 1x1+3+5"
                             "red1.png matches greenish.png at 1x1+3+8"
                             "red1.png matches greenish.png at 1x1+4+2"
                             "red1.png matches greenish.png at 1x1+4+4"
                             "red1.png matches greenish.png at 1x1+5+3"
                             "red1.png matches greenish.png at 1x1+5+6"
                             "red1.png matches greenish.png at 1x1+6+4"
                             "red1.png matches greenish.png at 1x1+6+8"
                             "red1.png matches greenish.png at 1x1+7+1"
                             "red1.png matches greenish.png at 1x1+7+5"
                             "red1.png matches greenish.png at 1x1+8+7"
                             "red1.png matches greenish.png at 1x1+9+3"))

   (spims-matches-check "../spims -pdir images/A5/Patterns -s images/A5/Sources/greenish.png"
                        (set "green23.png matches greenish.png at 2x3+0+0"
                             "green23.png matches greenish.png at 2x3+0+6"
                             "green23.png matches greenish.png at 2x3+1+1"
                             "green23.png matches greenish.png at 2x3+1+7"
                             "green23.png matches greenish.png at 2x3+2+2"
                             "green23.png matches greenish.png at 2x3+4+7"
                             "green23.png matches greenish.png at 2x3+5+0"
                             "green23.png matches greenish.png at 2x3+7+2"
                             "green23.png matches greenish.png at 2x3+8+0"
                             "green23.png matches greenish.png at 2x3+8+4"
                             "red1.png matches greenish.png at 1x1+0+3"
                             "red1.png matches greenish.png at 1x1+0+5"
                             "red1.png matches greenish.png at 1x1+0+9"
                             "red1.png matches greenish.png at 1x1+1+4"
                             "red1.png matches greenish.png at 1x1+2+0"
                             "red1.png matches greenish.png at 1x1+2+6"
                             "red1.png matches greenish.png at 1x1+3+1"
                             "red1.png matches greenish.png at 1x1+3+5"
                             "red1.png matches greenish.png at 1x1+3+8"
                             "red1.png matches greenish.png at 1x1+4+2"
                             "red1.png matches greenish.png at 1x1+4+4"
                             "red1.png matches greenish.png at 1x1+5+3"
                             "red1.png matches greenish.png at 1x1+5+6"
                             "red1.png matches greenish.png at 1x1+6+4"
                             "red1.png matches greenish.png at 1x1+6+8"
                             "red1.png matches greenish.png at 1x1+7+1"
                             "red1.png matches greenish.png at 1x1+7+5"
                             "red1.png matches greenish.png at 1x1+8+7"
                             "red1.png matches greenish.png at 1x1+9+3"))

   (spims-matches-check "../spims -p images/A5/Patterns/green23.png -sdir images/A4/Sources" (set))

   (spims-matches-check "../spims -pdir images/A5/Patterns -sdir images/A4/Sources"
                        (set "red1.png matches aa0018.jpg at 1x1+1165+1151"
                             "red1.png matches ad0001.jpg at 1x1+731+276"
                             "red1.png matches ae0007.jpg at 1x1+1382+471"
                             "red1.png matches af0002.jpg at 1x1+188+894"
                             "red1.png matches ag0001.jpg at 1x1+801+893"
                             "red1.png matches ag0009.jpg at 1x1+560+880"
                             "red1.png matches ai0153.jpg at 1x1+526+948"
                             "red1.png matches aj0021.jpg at 1x1+255+616"
                             "red1.png matches aj0021.jpg at 1x1+597+657"
                             "red1.png matches aj0057.jpg at 1x1+407+352"
                             "red1.png matches aj0072.jpg at 1x1+260+320"
                             "red1.png matches aj0072.jpg at 1x1+268+309"
                             "red1.png matches aj0085.jpg at 1x1+158+421"
                             "red1.png matches aj0129.jpg at 1x1+164+456"
                             "red1.png matches aj0154.jpg at 1x1+1547+879"
                             "red1.png matches aj0161.jpg at 1x1+394+246"
                             "red1.png matches ak432.png at 1x1+320+114"
                             "red1.png matches ap0201.jpg at 1x1+65+912"
                             "tree.jpg matches aa0010.jpg at 405x296+604+708"))))
