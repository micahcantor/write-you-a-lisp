(define-macro (when pred body)
  `(if ,pred
       ,body
       ()))

(print (when #t "true"))