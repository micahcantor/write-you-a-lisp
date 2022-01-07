(define first car)

(define (second lst)
  (car (cdr lst)))
  
(define (third lst)
  (car (cdr (cdr lst))))

(define-macro (infix expr)
  `(,(second expr) ,(first expr) ,(third expr)))

(infix (1 + 1))