(define a "global")

(let ()
  (define (showA)
    (print a))
  (showA)
  (define a "block")
  (showA))
