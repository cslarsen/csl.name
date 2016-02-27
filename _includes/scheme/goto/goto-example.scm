(import (scheme base)
        (scheme write)
        (print)
        (goto))

(define *age* #f)
(define *name* #f)

(define (print-person name age)
  (println (set-label *name* name) " is "
           (set-label *age* age) " years old."))

(print-person "John Doe" "123")
(goto-label *name* "Jane Doe")
(goto-label *age* "500")
