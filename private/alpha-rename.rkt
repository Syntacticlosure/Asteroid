#lang racket
(require "asteroid.rkt" syntax/parse/define)
(provide VarRef VarDef default-alpha-rename)
(define VarRef symbol?)
(define VarDef symbol?)


(define-simple-macro (default-alpha-rename (tag f:aster-field ...))
  #:do [(define alpha-rename-ref
          (λ (x)
            #`(hash-ref env #,x)))
        (define alpha-rename-def
          (λ (x)
            #`(let ([var (gensym 'x)])
                (set! new-env (hash-set new-env #,x var))
                var)))
        (define reify
          (λ (x)
            #`(#,x new-env)))]
  #:with ((~var varref (aster-field alpha-rename-ref #:recur-on-annot #'VarRef)) ...) #'(f ...)
  #:with ((~var vardef (aster-field alpha-rename-def #:recur-on-annot #'VarDef)) ...) #'(f ...)
  #:with ((~var field (aster-field reify)) ...) #'(f ...)
  [(and ast (tag f.name ...)) (λ (env)
                                (define new-env env)
                                (define def-alpha-renamed
                                  (match ast
                                    [(tag vardef.name ...) (tag vardef.wrapped ...)]))
                                (define ref-alpha-renamed
                                  (match def-alpha-renamed
                                    [(tag varref.name ...) (tag varref.wrapped ...)]))
                                (match ref-alpha-renamed
                                  [(tag field.name ...) (tag field.wrapped ...)]))])



