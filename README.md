# Asteroid
finally tagless style with initial encodings

# Usage
define a new language by `define-asteroid`:
```racket
(define-asteroid BaseArith
  (lit [c number?])
  (add lhs rhs)
  (sub lhs rhs))

(define-asteroid MulArith
  (mul lhs rhs)
  (div lhs rhs))
```

`define-asteroid` generates structs and catamorphism functions automatically.
To exports all generated structs and catamorphisms, use `asteroid-out`:

```racket
(provide (asteroid-out BaseArith)
         (asteroid-out MulArith))
```

define an evaluator for existing languages by `define-satellite`:

```racket
(define-satellite evaluate BaseArith
  [(lit x) x]
  [(add x y) (+ x y)]
  [(sub x y) (- x y)])

;; incremental programming
(define-satellite evaluate MulArith
  [(mul x y) (* x y)]
  [(div x y) (/ x y)])

(evaluate (add (lit 1) (mul (lit 2) (lit 3))))
;; ouptut: 7
```

# Metaprogramming
generate match clauses by metaprogramming:

```racket
(define-simple-macro (gen-to-sexpr (tag x:aster-field ...))
  [(tag x.name ...) `(tag ,x.name ...)])

(define-artificial-satellite to-sexpr BaseArith gen-to-sexpr)
(define-artificial-satellite to-sexpr MulArith gen-to-sexpr)

(to-sexpr (add (mul (lit 1) (lit 2)) (div (lit 2) (lit 1))))
;; output: '(add (mul (lit 1) (lit 2)) (div (lit 2) (lit 1)))
```
compared to nanopass, asteroid could control the process of automatic clause generation in a more powerful way.