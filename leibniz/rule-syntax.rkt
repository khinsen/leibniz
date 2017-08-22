#lang racket

(provide rules with-signature eq tr
         [rename-out [ts:T T]
                     [ts:term term]
                     [ts:pattern pattern]])

(require "./equations.rkt"
         (prefix-in ts: "./term-syntax.rkt")
         threading
         racket/stxparam
         (for-syntax syntax/parse
                     racket/stxparam))

(module+ test
  (require rackunit))

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
  (foldl (λ (vd vs) (hash-set vs (car vd) (cdr vd)))
         vars
         var-defs))

(define-syntax (rules stx)
  (syntax-parse stx
    [(_ signature
        (~var rule-defs (rule #'signature #'vars*)) ...)
     #`(~> empty-rulelist
           (add-rule
            (let ([vars* (local-vars (hash) rule-defs.vars)])
              rule-defs.expr))
           ...)]))

(define-syntax-parameter eq
  (λ (stx)
    (raise-syntax-error 'eq "eq keyword used outside with-signature" stx)))

(define-syntax-parameter tr
  (λ (stx)
    (raise-syntax-error 'tr "tr keyword used outside with-signature" stx)))

(define-syntax (with-signature stx)
  (syntax-parse stx
    [(_ signature:expr body:expr ...)
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
                          (apply make-rule (append pr.args '(#f)))))]))])
         (ts:with-signature signature
           body ...))]))
