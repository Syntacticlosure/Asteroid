#lang racket
(require syntax/parse/define)
(define-simple-macro (bounce-private file:string ...)
  #:with (file-path ...) (map (λ (f) (string-append "private/" (syntax-e f)))
                              (syntax-e #'(file ...)))
  (begin (require file-path) ...
         (provide (all-from-out file-path)) ...))

(bounce-private "asteroid.rkt"
                "alpha-rename.rkt"
                "default-transform.rkt")



