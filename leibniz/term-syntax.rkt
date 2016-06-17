#lang racket

(require "./terms.rkt"
         racket/stxparam
         (for-syntax syntax/parse
                     racket/stxparam))

(module+ test

  (require "./sorts.rkt"
           "./operators.rkt"
           "./builtins.rkt"
           rackunit
           racket/function
           rackjure/threading)

  (define sorts
    (~> exact-number-sorts
        (add-sort 'A) (add-sort 'B)
        (add-subsort-relation 'B 'A)
        (add-sort 'X) (add-sort 'Y)
        (add-subsort-relation 'Y 'X)))
  (define a-signature
    (~> (merge-signatures (empty-signature sorts)
                          exact-number-signature)
        (add-op 'an-A empty 'A)
        (add-op 'a-B empty 'B)
        (add-op 'an-X empty 'X)
        (add-op 'a-Y empty 'Y)
        (add-op 'foo empty 'B)
        (add-op 'foo (list 'B) 'A)
        (add-op 'foo (list 'A 'B) 'A))))

;
; Rough validation. This needs to be improved as the
; term construction API gets clearer.
;
(define (validate signature value)
  (unless value
    (error "Undefined operator in term"))
  (if (allowed-term? signature value)
      value
      (error "illegal builtin term type")))

;
; Basic term construction syntax
;
(begin-for-syntax
  
  (define-syntax-class (atom sig-var)
    #:description "atomic term"
    #:attributes (value)
    (pattern s:str #:with value #`(validate #,sig-var s))
    (pattern ((~literal quote) symbol:id)
             #:with value #`(validate #,sig-var (quote symbol)))
    (pattern x:number #:when (exact? (syntax-e #'x))
             #:with value #`(validate #,sig-var x)))

  (define-syntax-class (term sig-var)
    #:description "term"
    #:attributes (value)
    (pattern (~var a (atom sig-var)) #:with value #'a.value)
    (pattern symbol:id
             #:with value
             #`(validate #,sig-var (make-term #,sig-var (quote symbol) empty)))
    (pattern (symbol:id (~var arg-terms (term sig-var)) ...)
             #:with value
             #`(validate #,sig-var (make-term #,sig-var (quote symbol)
                                              (list arg-terms.value ...))))))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ signature:expr (~var t (term #'sig)))
     #'(let ([sig signature])
         t.value)]))

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
  (check-exn exn:fail? (thunk (term a-signature "foo"))))

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
                   [(_ (~var t (term #'sig)))
                    #'t.value]))])
           body ...))]))

(module+ test
  (check-equal? (term a-signature 2)
                (with-signature a-signature (T 2)))
  (check-equal? (term a-signature an-A)
                (with-signature a-signature (T an-A)))
  (with-signature a-signature
    (check-equal? (T 2) 2)
    (check-equal? (T (foo (foo a-B) a-B))
                  (term a-signature (foo (foo a-B) a-B)))))
