#lang racket

(provide with-context R T RT A S
         (rename-out [c:define-context define-context]
                     [c:context context]
                     [c:eq eq]
                     [c:tr tr]))

(require (prefix-in c: "./contexts.rkt")
         "./terms.rkt"
         "./term-syntax.rkt"
         "./equations.rkt"
         "./rewrite-compatibility.rkt"
         racket/stxparam
         (for-syntax syntax/parse
                     racket/stxparam))

(module+ test
  (require chk
           (only-in "./contexts.rkt" define-context eq tr))
  (define-context test-context
    (sort boolean)
    (op true boolean)
    (op false boolean)
    (op (not boolean) boolean)
    (op foo boolean)
    (op bar boolean)
    (=> (not true) false)
    (=> (not false) true)
    (=> foo (not true) #:if false)
    (=> foo (not false) #:if true)))

(define (reduce-anything context x)
  (cond
    [(term? x)
     (reduce context x)]
    [(equation? x)
     (reduce-equation context x)]
    [else
     (error (format "cannot reduce ~s" x))]))

(define (transform-anything context tr x)
  (cond
    [(term? x)
     (transform context tr x)]
    [(equation? x)
     (transform-equation context tr x)]
    [else
     (error (format "cannot transform ~s" x))]))

(define (substitute-anything context tr x)
  (cond
    [(term? x)
     (substitute context tr x)]
    [(equation? x)
     (substitute-equation context tr x)]
    [else
     (error (format "cannot substitute ~s" x))]))

(define-syntax-parameter R
  (λ (stx)
    (raise-syntax-error 'R "R keyword used outside with-context" stx)))

(define-syntax-parameter RT
  (λ (stx)
    (raise-syntax-error 'RT "RT keyword used outside with-context" stx)))

(define-syntax-parameter A
  (λ (stx)
    (raise-syntax-error 'A "A keyword used outside with-context" stx)))

(define-syntax-parameter S
  (λ (stx)
    (raise-syntax-error 'S "S keyword used outside with-context" stx)))

(define-syntax (with-context stx)
  (syntax-parse stx
    [(_ context:expr body:expr ...)
     #'(syntax-parameterize
           ([R (λ (stx)
                 (syntax-parse stx
                   [(_ arg:expr)
                    #'(reduce-anything context arg)]))]
            [RT (λ (stx)
                  (syntax-parse stx
                    [(_ term)
                     #'(reduce context (T term))]))]
            [A (λ (stx)
                 (syntax-parse stx
                   [(_ tr:expr arg:expr)
                    #'(transform-anything context tr arg)]))]
            [S (λ (stx)
                 (syntax-parse stx
                   [(_ tr:expr arg:expr)
                    #'(substitute-anything context tr arg)]))])
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
     #:= (R (eq foo true))
         (eq true true)
     #:= (R (eq foo true))
         (eq true true)
     #:= (A (tr #:var (X boolean) X (not X)) (T bar))
         (T (not bar))
     #:= (A (tr #:var (X boolean) X (not X)) (T foo))
         (T (not foo))
     #:= (A (tr #:var (X boolean) X (not X)) (eq bar foo))
         (eq (not bar) (not foo))
     #:= (A (tr #:var (X boolean) X (not X)) (eq bar foo))
         (eq (not bar) (not foo))
     #:= (S (tr bar (not bar))
            (T (not bar)))
         (T (not (not bar)))
     #:= (S (tr foo (not foo))
            (T (not foo)))
         (T (not (not foo)))
     #:= (S (tr foo (not bar))
            (eq foo bar))
         (eq (not bar) bar))))
