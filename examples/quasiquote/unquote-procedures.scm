(define (get-day)
  "Tuesday")
(define (get-month)
  "January")
(define (get-year)
  "2021")

(define (create-shipping-employee-association name)
  `((name ,name)
    (hire-date ,(get-day) ,(get-month) ,(get-year))))

(print (create-shipping-employee-association "Mark"))