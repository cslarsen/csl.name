(import (scheme base)
        (scheme write)
        (print)
        (goto))

(let* ((label #f)
       (n (set-label label 0)))
  (println n)
  (goto-label label (+ n 1)))
