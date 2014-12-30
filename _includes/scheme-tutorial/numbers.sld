(define-library (numbers)
  (export square)
  (import (scheme base))
  (begin
   (define (square n)
    (* n n))))
