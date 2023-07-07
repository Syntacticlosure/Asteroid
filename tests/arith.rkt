#lang racket
(require "../main.rkt" syntax/parse/define)
(provide (asteroid-out MulArith)
         (asteroid-out BaseArith)
         print)

(define-asteroid BaseArith
  (lit [c : number?])
  (add lhs rhs)
  (sub lhs rhs))

(define-asteroid MulArith
  (mul lhs rhs)
  (div lhs rhs))

(add (mul (lit 1) (lit 2)) (sub (lit 2) (lit 0)))

(define-satellite print BaseArith
  [(lit x) x]
  [(add x y) `(+ ,x ,y)]
  [(sub x y) `(- ,x ,y)])

(print (add (lit 1) (lit 2)))

(define-simple-macro (gen-print (tag x:aster-field ...))
  [(tag x.name ...) `(tag ,x.name ...)])

;; get print for MulArith for free
(define-artificial-satellite print MulArith (gen-print))

(print (add (mul (lit 1) (lit 2)) (div (lit 2) (lit 1))))

(define-satellite evaluate BaseArith
  [(lit x) x]
  [(add x y) (+ x y)]
  [(sub x y) (- x y)])

(define-satellite evaluate MulArith
  [(mul x y) (* x y)]
  [(div x y) (/ x y)])

(evaluate (add (lit 1) (mul (lit 2) (lit 3))))

(define-simple-macro (gen-to-sexpr (tag x:aster-field ...))
  [(tag x.name ...) `(tag ,x.name ...)])

(define-artificial-satellite to-sexpr BaseArith (gen-to-sexpr))
(define-artificial-satellite to-sexpr MulArith (gen-to-sexpr))

(to-sexpr (add (mul (lit 1) (lit 2)) (div (lit 2) (lit 1))))
