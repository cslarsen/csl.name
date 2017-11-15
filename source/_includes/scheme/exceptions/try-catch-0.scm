(import (scheme base)
        (scheme write)
        (print))

(define throw (lambda () #f))

(define-syntax try
  (syntax-rules (catch)
    ((try (catch thunk) body ...)
     (call/cc
       (lambda (exit)
         (set! throw
           (lambda (error)
             (println "Exception: " error)
             (exit)))
         body ...)))))

(try
  (catch
    (lambda (error)
      (println "Error: " error)))

  (define (divide a b)
    (println "About to perform " a "/" b)
    (if (zero? b)
      (throw "Divide by zero")
      (println a "/" b " = " (/ a b))))

  (divide 10 2)
  (divide 1 3)
  (divide 3 0))

(println "End of program")
