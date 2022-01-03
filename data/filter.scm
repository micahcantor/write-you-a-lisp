(define (filter pred? xs)
  (if (null? xs)
      ()
      (if (pred? (car xs))
          (cons (car xs) (filter pred? (cdr xs)))
          (filter pred? (cdr xs)))))

(filter (lambda (x) (== x 2)) '(1 2 3))