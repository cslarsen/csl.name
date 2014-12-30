(define-library (numbers)
  (import (scheme base))
  (export cube)
  (begin
    (define (cube n)
      (* n n n))))
