#lang racket

(provide with-context with-rules rules
         R T RT A S
         (rename-out [c:define-context define-context]
                     [c:context context]
                     [c:eq eq]
                     [c:tr tr]))

(require (prefix-in c: "./contexts.rkt")
         (only-in "./contexts.rkt" eq tr)
         "./terms.rkt"
         (prefix-in ts: "./term-syntax.rkt")
         (only-in "./term-syntax.rkt" T)
         "./equations.rkt"
         "./rewrite.rkt"
         (prefix-in compat: "./rewrite-compatibility.rkt")
         threading
         racket/stxparam
         (for-syntax syntax/parse
                     racket/stxparam))

(module+ test
  (require chk
           (only-in "./contexts.rkt" define-context eq tr)
           (prefix-in contexts: "./contexts.rkt"))
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
    (=> foo (not false) #:if true))
  (define test-signature (contexts:context-signature test-context))
  (define test-rules (contexts:context-rules test-context)))

(define (compat:reduce-anything context x)
  (cond
    [(term? x)
     (compat:reduce context x)]
    [(equation? x)
     (compat:reduce-equation context x)]
    [else
     (error (format "cannot reduce ~s" x))]))

(define (compat:transform-anything context tr x)
  (cond
    [(term? x)
     (compat:transform context tr x)]
    [(equation? x)
     (compat:transform-equation context tr x)]
    [else
     (error (format "cannot transform ~s" x))]))

(define (compat:substitute-anything context tr x)
  (cond
    [(term? x)
     (compat:substitute context tr x)]
    [(equation? x)
     (compat:substitute-equation context tr x)]
    [else
     (error (format "cannot substitute ~s" x))]))

(define (reduce-anything signature rules x)
  (cond
    [(term? x)
     (reduce signature rules x)]
    [(equation? x)
     (reduce-equation signature rules x)]
    [else
     (error (format "cannot reduce ~s" x))]))

(define (transform-anything signature tr x)
  (cond
    [(term? x)
     (transform signature tr x)]
    [(equation? x)
     (transform-equation signature tr x)]
    [else
     (error (format "cannot transform ~s" x))]))

(define (substitute-anything signature tr x)
  (cond
    [(term? x)
     (substitute signature tr x)]
    [(equation? x)
     (substitute-equation signature tr x)]
    [else
     (error (format "cannot substitute ~s" x))]))

(define-syntax-parameter R
  (λ (stx)
    (raise-syntax-error 'R "R keyword used outside with-rules" stx)))

(define-syntax-parameter RT
  (λ (stx)
    (raise-syntax-error 'RT "RT keyword used outside with-rules" stx)))

(define-syntax-parameter A
  (λ (stx)
    (raise-syntax-error 'A "A keyword used outside with-rules" stx)))

(define-syntax-parameter S
  (λ (stx)
    (raise-syntax-error 'S "S keyword used outside with-rules" stx)))

(define-syntax (with-context stx)
  (syntax-parse stx
    [(_ context:expr body:expr ...)
     #'(syntax-parameterize
           ([R (λ (stx)
                 (syntax-parse stx
                   [(_ arg:expr)
                    #'(compat:reduce-anything context arg)]))]
            [RT (λ (stx)
                  (syntax-parse stx
                    [(_ term)
                     #'(compat:reduce context (T term))]))]
            [A (λ (stx)
                 (syntax-parse stx
                   [(_ tr:expr arg:expr)
                    #'(compat:transform-anything context tr arg)]))]
            [S (λ (stx)
                 (syntax-parse stx
                   [(_ tr:expr arg:expr)
                    #'(compat:substitute-anything context tr arg)]))])
         (c:with-context context
           body) ...)]))

(begin-for-syntax

  (define-splicing-syntax-class opt-label
    #:description "optional label in a rule"
    (pattern (~seq #:label a-symbol:id)
             #:with expr #'(quote a-symbol))
    (pattern (~seq)
             #:with expr #'#f))

  (define-splicing-syntax-class opt-vars
    #:description "optional variable declaration in a rule"
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

  (define-splicing-syntax-class (embedded-pattern sig var)
    #:description "embedded pattern in a rule"
    (pattern p:expr
             #:with expr #`(ts:pattern #,sig #,var p)))

  (define-splicing-syntax-class (opt-condition sig vars)
    #:description "optional condition in a rule"
    (pattern (~seq #:if (~var condition (embedded-pattern sig vars)))
             #:with expr #'condition.expr)
    (pattern (~seq)
             #:with expr #'#f))

  (define-splicing-syntax-class (rule-hp sig vars)
    (pattern  (~seq a-label:opt-label
                    local-vars:opt-vars
                    (~var pattern (embedded-pattern sig vars))
                    (~var replacement (embedded-pattern sig vars))
                    (~var condition (opt-condition sig vars)))
              #:with args #`(list #,sig pattern.expr condition.expr
                                  replacement.expr a-label.expr)
              #:with vars #'local-vars.expr))

  (define-syntax-class (pattern-rule sig vars)
    #:description "pattern rule declaration"
    (pattern ((~datum =>)
              (~var prhp (rule-hp sig vars)))
             #:with expr #`(apply make-rule (append prhp.args '(#t)))
             #:with vars #'prhp.vars))

  (define-syntax-class (fn-rule sig vars)
    #:description "function rule declaration"
    (pattern ((~datum ->)
              a-label:opt-label
              local-vars:opt-vars
              (~var pattern (embedded-pattern sig vars))
              replacement:expr
              (~var condition (opt-condition sig vars)))
             #:with expr #`(make-rule #,sig pattern.expr condition.expr
                                      replacement a-label.expr #f)
             #:with vars #'local-vars.expr))

  (define-syntax-class (equation sig vars)
    #:description "equation declaration"
    (pattern ((~datum eq)
              local-vars:opt-vars
              (~var left (embedded-pattern sig vars))
              (~var right (embedded-pattern sig vars))
              (~var condition (opt-condition sig vars)))
             #:with expr #`(make-equation #,sig left.expr condition.expr
                                          right.expr)
             #:with vars #'local-vars.expr))
  (define-syntax-class (rule sig vars)
    #:description "rule declaration"
    (pattern (~var pr (pattern-rule sig vars))
             #:with expr #'pr.expr
             #:with vars #'pr.vars)
    (pattern (~var fr (fn-rule sig vars))
             #:with expr #'fr.expr
             #:with vars #'fr.vars)))

(define (local-vars vars var-defs)
  (foldl (λ (vd vs) (hash-set vs (car vd) (cdr vd))) vars var-defs))

(define-syntax (rules stx)
  (syntax-parse stx
    [(_ signature
        (~var rule-defs (rule #'signature #'vars*)) ...)
     #`(~> empty-rulelist
           (add-rule
            (let ([vars* (local-vars (hash) rule-defs.vars)])
              rule-defs.expr))
           ...)]))

(module+ test
  (define some-rules
    (rules test-signature
           (=> (not true) false)
           (=> (not false) true)
           (=> foo (not true) #:if false)
           (=> foo (not false) #:if true)))
  (chk #:= some-rules test-rules))

(define-syntax (with-rules stx)
  (syntax-parse stx
    [(_ signature:expr rules:expr body:expr ...)
     #'(syntax-parameterize
           ([eq (λ (stx)
                  (syntax-parse stx
                    [(~var eqn (equation #'signature
                                         #'vars*))
                     #'(let ([vars* (local-vars (hash) eqn.vars)])
                         eqn.expr)]))]
            [tr (λ (stx)
                  (syntax-parse stx
                    [(_  (~var pr (rule-hp #'signature
                                           #'vars*)))
                     #'(let ([vars* (local-vars (hash) pr.vars)])
                         (make-transformation
                          signature
                          (apply make-rule (append pr.args '(#f)))))]))]
            [R (λ (stx)
                 (syntax-parse stx
                   [(_ arg:expr)
                    #'(reduce-anything signature rules arg)]))]
            [RT (λ (stx)
                  (syntax-parse stx
                    [(_ term)
                     #'(reduce signature rules (T term))]))]
            [A (λ (stx)
                 (syntax-parse stx
                   [(_ tr:expr arg:expr)
                    #'(transform-anything signature tr arg)]))]
            [S (λ (stx)
                 (syntax-parse stx
                   [(_ tr:expr arg:expr)
                    #'(substitute-anything signature tr arg)]))])
         (ts:with-signature signature
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

(module+ test
  (with-rules test-signature test-rules
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
