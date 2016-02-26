(import (scheme base)
        (scheme write))

; A handy function that prints all of its arguments
(define (println . args)
  (for-each display args)
  (newline))

; ... insert set-label and goto-label here

(define label #f)

(define (print-person name age)
  (println name " is " (set-label age) " years old."))

(print-person "John Doe" 123)
(goto-label 500)
