(define (add1 x)
  (+ 1 x))

(define (map f xs) 
  (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))))

(print (map add1 '(1 2 3 4 5)))
