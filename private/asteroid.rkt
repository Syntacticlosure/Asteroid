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
  (define-syntax-class (aster-field [recursive-wrapper values])
    (pattern name:id
      #:attr annot #'any/c
      #:attr wrapped (recursive-wrapper #'name))
    (pattern [name:id annot]
      #:attr wrapped #'name))
  (define (generate-field-recur recur-stx field)
    (syntax-parse field
      [field:id #`(#,recur-stx field)]
      [field:aster-field #`field.name]))
  (define (generate-fields-recurs recur-stx fields)
    (datum->syntax #'ctx
                   (map (curry generate-field-recur recur-stx) (syntax-e fields))))
  (struct asteroid-static-infos (cata-id forms) #:transparent))


(define-syntax (define-asteroid stx)
  (syntax-parse stx
    [(_ aster-name:id (tag:id field:aster-field ...) ...)
     #:with ((recur-field ...) ...) (datum->syntax #'ctx
                                                   (map (curry generate-fields-recurs #'(self f))
                                                        (syntax-e #'((field ...) ...))))
     #`(begin
         (define-syntax aster-name (asteroid-static-infos #'aster-cata (quote-syntax ((tag field ...) ...))))
         (struct/contract tag ([field.name field.annot] ...) #:transparent)
         ...
         (define ((((aster-cata extension) self) f) ast)
           (match ast
             [(tag field.name ...) (f (tag recur-field ...))] ...
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
         #'(combine-out (struct-out aster-struct) ...)
         modes)]))))


(define (fix f)
  (f (λ (x)
       ((fix f) x))))

(define (((base-cata self) f) ast)
  (error "no matching clause for catamorphism"))

(define (combine-cata cata-set)
  (define cata-list (set->list cata-set))
  (fix (foldr (λ (x y)
                (x y)) base-cata cata-list)))

(define (base-dispatcher ast)
  (error "no match clause for dispatcher"))

(begin-for-syntax
  (struct satellite-static-infos (asteroids-box cata-box dispatcher-box transformer)
    #:property prop:procedure (struct-field-index transformer) #:transparent)
  (define (init-satellite stx)
    (syntax-parse stx
      [(_ satellite-name:id aster-name:id match-clauses ...)
       #`(begin
           (define-for-syntax cata-box (box #'base-cata))
           (define-for-syntax dispatcher-box (box #'base-dispatcher))
           (define-syntax satellite-name (satellite-static-infos (box (list)) cata-box dispatcher-box
                                                                 (λ (stx)
                                                                   (define satellite-v #`((fix #,(unbox cata-box)) #,(unbox dispatcher-box)))
                                                                   (syntax-parse stx
                                                                     [x:id satellite-v]
                                                                     [(x args (... ...)) #`(#,satellite-v args (... ...))])))))]))
  (define (satellite-add-new-asteroid! satellite-name asteroid)
    (define satellite-meta (syntax-local-value satellite-name))
    (match satellite-meta
      [(satellite-static-infos asteroids-box _ _ _)
       (set-box! asteroids-box (cons asteroid (unbox asteroids-box)))]))
  (define (satellite-update-cata! satellite-name cata-id)
    (define satellite-meta (syntax-local-value satellite-name))
    (match satellite-meta
      [(satellite-static-infos _ cata-box _ _)
       (set-box! cata-box cata-id)]))
  (define (satellite-update-dispatcher! satellite-name dispatcher-id)
    (define satellite-meta (syntax-local-value satellite-name))
    (match satellite-meta
      [(satellite-static-infos _ _ dispatcher-box _)
       (set-box! dispatcher-box dispatcher-id)])))

(define-syntax (define-satellite stx)
  (syntax-parse stx
    [(_ satellite-name:id aster-name:id match-clauses ...)
     #:do [(define satellite-meta (syntax-local-value #'satellite-name (λ () #f)))]
     #:when (not satellite-meta)
     #:do [(define satellite-def (init-satellite stx))]
     #`(begin
         #,satellite-def
         (define-satellite satellite-name aster-name match-clauses ...))]
    [(_ satellite-name:id aster-name:id match-clauses ...)
     #:do [(define satellite-meta (syntax-local-value #'satellite-name))
           (match-define (satellite-static-infos asteroids-box cata-box dispatcher-box trans) satellite-meta)
           (define old-asteroids (unbox asteroids-box))
           (define new-asteroid (not (memf (curry free-identifier=? #'aster-name) old-asteroids)))
           (define update-asteroids (if new-asteroid
                                        #`(begin-for-syntax (satellite-add-new-asteroid! #'satellite-name #'aster-name))
                                        #`(void)))

           (define old-cata (syntax-local-introduce (unbox cata-box)))
           (match-define (asteroid-static-infos aster-cata forms) (syntax-local-value #'aster-name))
           (define cata-def (if new-asteroid #`(define cata (#,aster-cata #,old-cata))
                                #`(begin)))
           (define update-cata #`(begin-for-syntax (satellite-update-cata! #'satellite-name (syntax-local-introduce #'cata))))
           
           (define old-dispatcher (syntax-local-introduce (unbox dispatcher-box)))
           (define update-dispatcher #`(begin-for-syntax (satellite-update-dispatcher! #'satellite-name  (syntax-local-introduce #'dispatcher))))]
     #`(begin
         (define (dispatcher ast)
           (match ast
             match-clauses ...
             [else (#,old-dispatcher ast)]))
         #,cata-def

         #,update-asteroids
         #,update-cata
         #,update-dispatcher
         )]))


(define-syntax (define-artificial-satellite stx)
  (syntax-parse stx
    [(_ satellite-name:id aster-name:id generater-macro:id)
     #:do [(match-define (asteroid-static-infos aster-cata aster-forms) (syntax-local-value #'aster-name))
           (define generater-proc (syntax-local-value #'generater-macro))
           (define match-clauses (datum->syntax #'ctx (for/list ([form (in-list (syntax-e aster-forms))])
                                                        (syntax-local-apply-transformer generater-proc
                                                                                        #'generater-macro
                                                                                        'expression
                                                                                        #f
                                                                                        #`(generater-macro #,form)))))]
     #`(define-satellite satellite-name aster-name
         #,@match-clauses)]))



