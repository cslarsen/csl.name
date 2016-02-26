(define-library (print)
  (import (scheme base)
          (scheme write))
  (export println)
  (begin
    (define (println . args)
      "Prints all arguments and a newline."
      (for-each display args)
      (newline))))
