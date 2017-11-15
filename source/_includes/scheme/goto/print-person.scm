(import (scheme base)
        (scheme write))

(define (println . args)
  (for-each display args)
  (newline))

(define (set-label initial-return-value)
  (call/cc
    (lambda (continuation)
      (set! label continuation)
      initial-return-value)))

(define (goto-label new-value)
  (label new-value))

(define label #f)

(define (print-person name age)
  (println name " is " (set-label age) " years old."))

(println "Value of label: " label)
(print-person "John Doe" 123)

(println "Value of label after calling print-person: " label)
(goto-label 500)

(println "Done!")
