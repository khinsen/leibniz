#lang racket

(provide with-context R T RT
         (rename-out [c:define-context define-context]))

(require (prefix-in c: "./contexts.rkt")
         "./term-syntax.rkt"
         "./rewrite.rkt"
         racket/stxparam
         (for-syntax syntax/parse
                     racket/stxparam))

(module+ test
  (require chk)
  (c:define-context test-context
    (sort Boolean)
    (op true Boolean)
    (op false Boolean)
    (op (not Boolean) Boolean)
    (op foo Boolean)
    (=> (not true) false)
    (=> (not false) true)
    (=> foo (not true) #:if false)
    (=> foo (not false) #:if true)))

(define-syntax-parameter R
  (λ (stx)
    (raise-syntax-error 'R "R keyword used outside with-context" stx)))

(define-syntax-parameter RT
  (λ (stx)
    (raise-syntax-error 'RT "RT keyword used outside with-context" stx)))

(define-syntax (with-context stx)
  (syntax-parse stx
    [(_ context:expr body:expr ...)
     #'(syntax-parameterize
           ([R (λ (stx)
                 (syntax-parse stx
                   [(_ term:expr)
                    #'(reduce context term)]))]
            [RT (λ (stx)
                  (syntax-parse stx
                    [(_ term)
                     #'(reduce context (T term))]))])
         (c:with-context context
           body) ...)]))

(module+ test
  (with-context test-context
    (chk
     #:= (RT (not true))        (T false)
     #:= (RT (not false))       (T true)
     #:= (RT (not (not false))) (T false)
     #:= (R (T foo))            (T true))))
