(define-macro (when pred body)
  `(if ,pred
       (begin ,body)
       ()))

(print (when #t "true"))