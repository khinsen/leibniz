#lang racket

(provide term pattern with-signature T)

(require "./terms.rkt"
         "./operators.rkt"
         racket/stxparam
         (for-syntax syntax/parse
                     racket/stxparam))

(module+ test
  (require "./test-examples.rkt"
           rackunit
           racket/function))

;
; Validation for atomic terms. op-terms are validated by make-term.
;
(define (validate-atomic signature value)
  (if (allowed-term? signature value)
      value
      (error (format "~s: builtin term type ~s not allowed by signature"
                     value (term.builtin-type value)))))

;
; Basic term construction syntax
;
(begin-for-syntax
  
  (define-syntax-class (atom sig-var)
    #:description "atomic term"
    #:attributes (value)
    (pattern s:str #:with value #`(validate-atomic #,sig-var s))
    (pattern ((~literal quote) symbol:id)
             #:with value #`(validate-atomic #,sig-var (quote symbol)))
    (pattern x:number
             #:with value #`(validate-atomic #,sig-var x)))

  (define-syntax-class (term sig-var)
    #:description "term"
    #:attributes (value)
    (pattern (~var a (atom sig-var)) #:with value #'a.value)
    (pattern symbol:id
             #:with value
             #`(make-term #,sig-var (quote symbol) empty))
    (pattern (symbol:id (~var arg-terms (term sig-var)) ...)
             #:with value
             #`(make-term #,sig-var (quote symbol)
                          (list arg-terms.value ...))))

  (define-splicing-syntax-class opt-vars
    #:description "optional variable declaration in a rule or equation"
    (pattern (~seq #:vars ([var-name:id var-sort:id] ...))
             #:with expr #'(list (cons (quote var-name)
                                       (quote var-sort)) ...))
    (pattern (~seq #:var [var-name:id var-sort:id])
             #:with expr #'(list (cons (quote var-name)
                                       (quote var-sort))))
    ; a more mathematics-like variant
    (pattern (~seq (~seq (~datum ∀) var-name:id (~datum :) var-sort:id) ...)
             #:with expr #'(list (cons (quote var-name)
                                       (quote var-sort)) ...))
    ; two variants of the former with parentheses for use with sweet-exp
    (pattern (~seq ((~seq (~datum ∀) var-name:id (~datum :) var-sort:id)) ...)
             #:with expr #'(list (cons (quote var-name)
                                       (quote var-sort)) ...))
    (pattern (~seq (~seq (~datum ∀) var-name-1:id (~datum :) var-sort-1:id)
                   ((~seq (~datum ∀) var-name:id (~datum :) var-sort:id)) ...)
             #:with expr #'(list (cons (quote var-name-1)
                                       (quote var-sort-1))
                                 (cons (quote var-name)
                                       (quote var-sort)) ...))
    (pattern (~seq)
             #:with expr #'empty))

  (define-syntax-class (term-pattern sig-var vars-var)
    #:description "pattern"
    #:attributes (value)
    (pattern (~var a (atom sig-var)) #:with value #'a.value)
    (pattern symbol:id
             #:with value
             #`(make-var-or-term #,sig-var (quote symbol) #,vars-var))
    (pattern (symbol:id (~var arg-terms (term-pattern sig-var vars-var)) ...)
             #:with value
             #`(make-term #,sig-var (quote symbol)
                          (list arg-terms.value ...)))))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ signature:expr (~var t (term #'sig)))
     #'(let ([sig signature])
         t.value)]))

(define (local-vars vars var-defs)
  (foldl (λ (vd vs) (hash-set vs (car vd) (cdr vd))) vars var-defs))

(define-syntax (pattern stx)
  (syntax-parse stx
    [(_ signature:expr lvars ov:opt-vars
        (~var p (term-pattern #'sig #'vars)))
     #'(let ([sig signature]
             [vars (local-vars lvars  ov.expr)])
         p.value)]))

(module+ test
  (check-equal?  (term a-signature 2) 2)
  (check-equal?  (term a-signature an-A)
                 (make-term a-signature 'an-A empty))
  (check-equal? (term a-signature (foo a-B))
                (make-term a-signature 'foo
                           (list (make-term a-signature 'a-B empty))))
  (check-equal? (term a-signature (foo (foo a-B) a-B))
                (let* ([a-B (make-term a-signature 'a-B empty)]
                       [foo-a-B (make-term a-signature 'foo (list a-B))])
                  (make-term a-signature 'foo (list foo-a-B a-B))))
  (check-exn exn:fail? (thunk (term a-signature 'foo)))
  (check-exn exn:fail? (thunk (term a-signature "foo")))
  (check-equal? (pattern a-signature (hash) Avar)
                (make-var a-signature 'Avar))
  (check-equal? (pattern a-signature (hash)  #:var (Xvar ℤ) Xvar)
                (make-var a-signature 'Xvar (hash 'Xvar 'ℤ)))
  (check-equal? (pattern a-signature (hash)  (foo Bvar))
                (make-term a-signature 'foo
                           (list (make-var a-signature 'Bvar)))))

;
; with-signature
;
(define-syntax-parameter T
  (λ (stx)
    (raise-syntax-error 'T "T keyword used outside with-signature" stx)))

(define-syntax (with-signature stx)
  (syntax-parse stx
    [(_ signature:expr body:expr ...)
     #'(let ([sig signature])
         (syntax-parameterize
             ([T (λ (stx)
                   (syntax-parse stx
                     [(_ ov:opt-vars (~var p (term-pattern #'sig #'vars)))
                      #'(let ([vars (local-vars (hash) ov.expr)])
                          p.value)]))])
           body ...))]))

(module+ test
  (check-equal? (term a-signature 2)
                (with-signature a-signature (T 2)))
  (check-equal? (term a-signature an-A)
                (with-signature a-signature (T an-A)))
  (with-signature a-signature
    (check-equal? (T 2) 2)
    (check-equal? (T (foo (foo a-B) a-B))
                  (term a-signature (foo (foo a-B) a-B)))
    (check-equal? (T (foo Bvar))
                  (pattern a-signature (hash) (foo Bvar)))))
