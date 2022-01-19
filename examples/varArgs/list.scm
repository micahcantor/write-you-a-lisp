(define (list & values)
  (if (null? values)
      '()
      (cons (car values) (cdr values))))

(print (list 1 2 3))