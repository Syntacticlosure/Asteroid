#lang racket
(require syntax/parse/define "asteroid.rkt")
(provide default-transform)
(define-syntax-parser default-transform
  [(default-transform (tag field ...)
     (~alt (~optional (~seq #:context (context ...)))
           (~optional (~seq #:reflect reflect:id)
                      #:defaults ([reflect #'values]))
           (~optional (~seq #:reify reify:id)
                      #:defaults ([reify #'values]))) ...)
   #:do [(define (recur v-stx)
           (if (attribute context)
               #`(reify (#,v-stx context ...))
               #`(reify #,v-stx)))]
   #:with ((~var parsed-field (aster-field recur)) ...) #`(field ...)
   #`[(tag parsed-field.name ...) #,(if (attribute context)
                                        #`(λ (context ...)
                                            (reflect (tag parsed-field.wrapped ...)))
                                        #`(reflect (tag parsed-field.wrapped ...)))]])