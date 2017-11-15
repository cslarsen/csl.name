(define-library (goto)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda))
  (export set-label
          goto-label)
  (begin
    (define goto-label
      (case-lambda
        ((label) (label '()))
        ((label value) (label value))))

    (define-syntax set-label
      (syntax-rules ()
        ((_ label value)
           (call/cc (lambda (k)
                      (if (not label) (set! label k))
                      value)))
        ((_ label)
           (set-label label '()))))))
