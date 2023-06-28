#lang racket
(require "../main.rkt"
         syntax/parse/define)

(define-asteroid Expr
  (Lit [n : number?])
  (Var [n : symbol?])
  (Lam [vars : (listof symbol?)] (list bodies))
  (App f (list args))
  (Begin (list bodies)))

(define-simple-macro (gen-transform (tag x:aster-field ...))
  [(tag x.name ...) (tag x.name ...)])

(define-artificial-satellite normalize-lam-body Expr gen-transform)

(define-satellite normalize-lam-body Expr
  [(Lam vars bodies) (Lam vars (list (Begin bodies)))])

(normalize-lam-body (App (Lam '(x y) (list (Var 'x) (Var 'y))) (list (Lit 1) (Lit 2))))