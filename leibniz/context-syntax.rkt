#lang racket

(provide with-context R T RT
         (rename-out [c:define-context define-context]
                     [c:eq eq]
                     [c:=> =>]))

(require (prefix-in c: "./contexts.rkt")
         "./terms.rkt"
         "./term-syntax.rkt"
         "./equations.rkt"
         "./rewrite.rkt"
         racket/stxparam
         (for-syntax syntax/parse
                     racket/stxparam))

(module+ test
  (require chk
           (only-in "./contexts.rkt" define-context eq =>))
  (define-context test-context
    (sort Boolean)
    (op true Boolean)
    (op false Boolean)
    (op (not Boolean) Boolean)
    (op foo Boolean)
    (=> (not true) false)
    (=> (not false) true)
    (=> foo (not true) #:if false)
    (=> foo (not false) #:if true)))

(define (reduce-anything context x [opt-label #f])
  (cond
    [(term? x)
     (reduce context x)]
    [(equation? x)
     (reduce-equation context x opt-label)]
    [else
     (error (format "cannot reduce ~s" x))]))

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
                   [(_ arg:expr)
                    #'(reduce-anything context arg)]
                   [(_ arg:expr (~seq #:label label:id))
                    #'(reduce-anything context arg (quote label))]
                   [(_ (~seq #:label label:id) arg:expr)
                    #'(reduce-anything context arg (quote label))]))]
            [RT (λ (stx)
                  (syntax-parse stx
                    [(_ term)
                     #'(reduce context (T term))]))])
         (c:with-context context
           body) ...)]))

(module+ test
  (with-context test-context
    (chk
     #:= (RT (not true))         (T false)
     #:= (RT (not false))        (T true)
     #:= (RT (not (not false)))  (T false)
     #:= (R (T foo))             (T true)
     #:= (R (eq foo true))       (eq true true)
     #:= (R (eq foo true) #:label label)
         (eq #:label label true true)
     #:= (R #:label label (eq foo true))
         (eq #:label label true true))))
