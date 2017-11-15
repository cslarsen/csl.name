(define-library (print)
  (import (scheme base)
          (scheme write))
  (export println)
  (begin
    (define (println . args)
      (for-each display args)
      (newline))))
