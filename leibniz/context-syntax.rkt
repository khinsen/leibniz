#lang racket

(provide with-context R T RT A
         (rename-out [c:define-context define-context]
                     [c:eq eq]
                     [c:tr tr]))

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
           (only-in "./contexts.rkt" define-context eq tr))
  (define-context test-context
    (sort Boolean)
    (op true Boolean)
    (op false Boolean)
    (op (not Boolean) Boolean)
    (op foo Boolean)
    (op bar Boolean)
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

(define (transform-anything context tr x [opt-label #f])
  (cond
    [(term? x)
     (transform context tr x)]
    [(equation? x)
     (transform-equation context tr x opt-label)]
    [else
     (error (format "cannot transform ~s" x))]))

(define-syntax-parameter R
  (λ (stx)
    (raise-syntax-error 'R "R keyword used outside with-context" stx)))

(define-syntax-parameter RT
  (λ (stx)
    (raise-syntax-error 'RT "RT keyword used outside with-context" stx)))

(define-syntax-parameter A
  (λ (stx)
    (raise-syntax-error 'A "A keyword used outside with-context" stx)))

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
                     #'(reduce context (T term))]))]
            [A (λ (stx)
                 (syntax-parse stx
                   [(_ tr:expr arg:expr)
                    #'(transform-anything context tr arg)]
                   [(_ tr:expr arg:expr (~seq #:label label:id))
                    #'(transform-anything context tr arg (quote label))]
                   [(_ tr:expr (~seq #:label label:id) arg:expr)
                    #'(transform-anything context tr arg (quote label))]
                   [(_ (~seq #:label label:id) tr:expr arg:expr)
                    #'(transform-anything context tr arg (quote label))]))])
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
         (eq #:label label true true)
     #:= (A (tr #:var (X Boolean) X (not X)) (T bar))
         (T (not bar))
     #:= (A (tr #:var (X Boolean) X (not X)) (T foo))
         (T false)
     #:= (A (tr #:var (X Boolean) X (not X)) (eq bar foo))
         (eq (not bar) false)
     #:= (A (tr #:var (X Boolean) X (not X)) (eq bar foo) #:label an-eq)
         (eq #:label an-eq (not bar) false))))
