#lang racket
(require (for-syntax syntax/parse
                     racket
                     (only-in racket/provide-transform make-provide-transformer
                              expand-export)))

(provide define-asteroid
         define-satellite
         define-artificial-satellite
         (for-syntax aster-field)
         asteroid-out)

(begin-for-syntax
  (define-syntax-class (aster-field [recursive-wrapper values] #:recur-on-annot [recur-on-annot #f])
    (pattern name:id
      #:attr annot #'any/c
      #:attr wrapped ((if recur-on-annot values recursive-wrapper) #'name))
    (pattern [name:id (~datum :) annot]
      #:attr wrapped (if
                      (and recur-on-annot (free-identifier=? #'annot recur-on-annot))
                      (recursive-wrapper #'name) #'name))
    (pattern [(~literal list) (~var x (aster-field recursive-wrapper))]
      #:with (n) (generate-temporaries #'(x))
      #:attr name #'n
      #:attr annot #'(listof x.annot)
      #:attr wrapped #`(map (λ (x.name) x.wrapped) n)))
  (struct asteroid-static-infos (cata-id forms) #:transparent))


(define-syntax (define-asteroid stx)
  (syntax-parse stx
    [(_ aster-name:id (tag:id (~var field (aster-field (λ (x) #`((self f) #,x)))) ...) ...)
     #`(begin
         (define-syntax aster-name (asteroid-static-infos #'aster-cata (quote-syntax ((tag field ...) ...))))
         (struct/contract tag ([field.name field.annot] ...) #:transparent)
         ...
         (define ((((aster-cata extension) self) f) ast)
           (match ast
             [(tag field.name ...) (f (tag field.wrapped ...))] ...
             [_ (((extension self) f) ast)])))]))


(define-syntax asteroid-out
  (make-provide-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ aster-name:id)
        #:do [(match-define (asteroid-static-infos aster-cata aster-forms) (syntax-local-value #'aster-name))
              (define structs
                (syntax-parse aster-forms
                  [((tag . rst) ...) #'(tag ...)]))]
        #:with (aster-struct ...) structs
        (expand-export
         #'(combine-out (struct-out aster-struct) ... aster-name)
         modes)]))))


(define (fix f)
  (f (λ (x)
       ((fix f) x))))

(define (((base-cata self) f) ast)
  (error (format "catamorphism: no matching clause for ~a" ast)))

(define (base-dispatcher ast)
  (error (format "dispatcher: no match clause for ~a" ast)))

(begin-for-syntax
  (struct satellite-static-infos (asteroids-box cata-box dispatcher-box transformer)
    #:property prop:procedure (struct-field-index transformer) #:transparent)
  (define (init-satellite stx)
    (syntax-parse stx
      [(_ satellite-name:id . _)
       #`(begin
           (define-for-syntax cata-box (box #'base-cata))
           (define-for-syntax dispatcher-box (box #'base-dispatcher))
           (define-syntax satellite-name (satellite-static-infos (box (list)) cata-box dispatcher-box
                                                                 (λ (stx)
                                                                   (define satellite-v #`((fix #,(unbox cata-box)) #,(unbox dispatcher-box)))
                                                                   (syntax-parse stx
                                                                     [x:id satellite-v]
                                                                     [(x args (... ...)) #`(#,satellite-v args (... ...))])))))])))

(define-syntax (define-satellite stx)
  (syntax-parse stx
    [(_ satellite-name:id)
     (init-satellite stx)]
    [(_ satellite-name:id aster-name:id match-clauses ...)
     #:do [(define satellite-meta (syntax-local-value #'satellite-name))
           (match-define (satellite-static-infos asteroids-box cata-box dispatcher-box trans) satellite-meta)
           (define old-asteroids (unbox asteroids-box))
           (define new-asteroid (not (memf (curry free-identifier=? #'aster-name) old-asteroids)))
           (define update-asteroids (if new-asteroid
                                        #`(set-box! asteroids-box (cons #'aster-name (unbox asteroids-box)))
                                        #`(void)))

           (define old-cata (syntax-local-introduce (unbox cata-box)))
           (match-define (asteroid-static-infos aster-cata forms) (syntax-local-value #'aster-name))
           (define cata-def (if new-asteroid #`(define cata (#,aster-cata #,old-cata))
                                #`(begin)))
           (define update-cata (if new-asteroid #`(set-box! cata-box (quote-syntax cata))
                                   #`(begin)))
           
           (define old-dispatcher (syntax-local-introduce (unbox dispatcher-box)))
           (define update-dispatcher #`(set-box! dispatcher-box (quote-syntax dispatcher)))]
     #`(begin
         (define (dispatcher ast)
           (match ast
             match-clauses ...
             [else (#,old-dispatcher ast)]))
         #,cata-def
         (begin-for-syntax
           (define satellite-meta (syntax-local-value #'satellite-name))
           (match-define (satellite-static-infos asteroids-box cata-box dispatcher-box _) satellite-meta)
           #,update-asteroids
           #,update-cata
           #,update-dispatcher
           ))]))


(define-syntax (define-artificial-satellite stx)
  (syntax-parse stx
    [(_ satellite-name:id aster-name:id (generater-macro:id . args))
     #:do [(match-define (asteroid-static-infos aster-cata aster-forms) (syntax-local-value #'aster-name))
           (define generater-proc (syntax-local-value #'generater-macro))
           (define match-clauses (datum->syntax #'ctx (for/list ([form (in-list (syntax-e aster-forms))])
                                                        (syntax-local-apply-transformer generater-proc
                                                                                        #'generater-macro
                                                                                        'expression
                                                                                        #f
                                                                                        #`(generater-macro #,form . args)))))]
     #`(define-satellite satellite-name aster-name
         #,@match-clauses)]
    [(_ satellite-name:id (aster-name:id ...) . rest)
     #`(begin
         (define-artificial-satellite satellite-name aster-name . rest) ...)]))

