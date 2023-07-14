#lang racket
(require "../main.rkt"
         "arith.rkt")


(print (add (lit 1) (lit 2)))
(define-satellite pprint)
(define-satellite pprint BaseArith
  [(lit c) c]
  [(add lhs rhs) `(+ ,lhs ,rhs)]
  [(sub lhs rhs) `(- ,lhs ,rhs)])

(pprint (add (lit 1) (lit 2)))