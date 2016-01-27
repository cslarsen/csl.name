(import (scheme base)
        (scheme write)
        (print))

; To be able to overwrite top-level definitions that are passed by value, we'll
; create a macro that expands and embeds the code below.
(define-syntax make-label
  (syntax-rules ()
    ((make-label variable initial-return-value)
      (call/cc (lambda (continuation)
        (if (not variable)              ; only set if label is uninitialized,
          (set! variable continuation)) ; otherwise jumping to age-label will
        initial-return-value)))))       ; overwrite name-label, and vice versa.

(define (continue-at continue-at-label value-to-pass)
  (continue-at-label value-to-pass))

; Placeholder for labels
(define age-label #f)
(define name-label #f)

(define (print-person name age)
  (println (make-label name-label name) " is "
           (make-label age-label age) " years old."))

; First call captures continuations, next calls jump back to them
(print-person "John Doe" 123)       ; "John Doe is 123 years old."
(continue-at age-label 500)         ; "John Doe is 500 years old."
(continue-at name-label "Jane Doe") ; "Jane Doe is 123 years old."
