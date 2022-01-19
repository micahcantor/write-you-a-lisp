# write-you-a-lisp

This is a work-in-progress repository for a small lisp written in Haskell. It is written primarily for clarity and concision, to show how to write an interpreter in Haskell using monad transformers.

The language is mostly a core subset of Scheme, with a few differences stolen from Clojure:

- There are no pairs, only lists.
- Variable arity functions/macros are written in the form `(params ... & varArg)` rather than the dotted Scheme sytnax.
- There is just one equality test, `=`.
- Has a Clojure-like `define-macro` form rather than Scheme's `syntax-rules`/`syntax-case`.

The implementation currently uses [relude](https://hackage.haskell.org/package/relude) (with the NoImplicitPrelude extension) rather than the standard Prelude.

## Examples

### Factorial
```scm
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(print (factorial 5))
```

### map
```scm
(define (add1 x)
  (+ 1 x))

(define (map f xs) 
  (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))))

(print (map add1 '(1 2 3 4 5)))
```

### list
```scm
(define (list & values)
  (if (null? values)
      '()
      (cons (car values) (cdr values))))

(print (list 1 2 3))
```

### quasiquotation/infix macro

```scm
(define first car)

(define (second lst)
  (car (cdr lst)))
  
(define (third lst)
  (car (cdr (cdr lst))))

(define-macro (infix expr)
  `(,(second expr) ,(first expr) ,(third expr)))

(print (infix (1 + 1)))
```