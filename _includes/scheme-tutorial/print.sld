(define-library (print)
  (import (scheme base)
          (scheme write))

  (export print println prints printsln)

  (begin
    ;; Prints each argument to the default output port
    (define (print . args)
      (for-each display args))

    ;; Prints each argument, separated by a single space, to the default output
    ;; port.
    (define (prints . args)
      (for-each (lambda (s)
                  (display s)
                  (display " ")) args))

    ;; Prints each argument to the default output port, terminated by a
    ;; newline character.
    (define (println . args)
      "Prints each argument, ending with a newline."
      (apply print args)
      (newline))

    ;; Prints each argument to the default output port, separated by single
    ;; spaces, ending with a newline character.
    (define (printsln . args)
      (apply prints args)
      (newline))))
