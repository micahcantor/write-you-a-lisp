(define add1
  (lambda (x)
    (+ 1 x)))

(define map 
  (lambda (f xs)
    (if (null? xs)
        ()
        (cons (f (car xs)) (map f (cdr xs))))))

(map add1 (cons 1 ()))
