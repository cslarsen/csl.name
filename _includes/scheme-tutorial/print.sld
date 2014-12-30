(define-library (print)
  (import (scheme base)
          (scheme write))

  (export print prints println printsln)

  (begin
    (define (print . args)
      "Prints each argument."
      (for-each display args))

    (define (prints . args)
      "Prints each argument separated by space."
      (for-each (lambda (s)
                  (display s)
                  (display " ")) args))

    (define (println . args)
      "Prints each argument, ending with a newline."
      (apply print args)
      (newline))

    (define (printsln . args)
      "Prints each argument and separate with space, ending with a newline."
      (apply prints args)
      (newline))))
