(import (scheme base)
        (scheme write)
        (srfi 69))

(define *labels*
 (make-hash-table eq?))

(define (label name)
 "Capture and name the current continuation."
 (call/cc (lambda (here)
           (hash-table-set! *labels* name here))))

(define (goto name)
 "Jump to given continuation and pass optional arguments."
 ((hash-table-ref *labels* name)))

;; tests

(label 'out)
(let ((n 0))
  (label 'start)
  (display (string-append "n=" (number->string n) "\n"))
  (set! n (+ 1 n))
  (if (> n 10) (goto 'out))
  (goto 'start))
(display "end\n")
