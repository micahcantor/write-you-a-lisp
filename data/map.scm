(define map 
  (lambda (f xs)
    (if (null? xs)
        ()
        (cons (f (car xs)) (map f (cdr xs))))))

(map (cons 1 (cons 2 (cons 3 ()))))