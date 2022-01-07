(define-macro (when pred body)
  `(if ,pred
       ,body
       ()))

(when #t "true")