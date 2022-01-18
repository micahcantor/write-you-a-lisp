(define x 1)
(define (f) x)
(define (g) (let ((x 2)) (f)))
(print (g))