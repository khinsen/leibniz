#lang racket

(provide 
 (rename-out [is-context? context?]
             [context- context])
 define-context
 with-context tr eq
 (contract-out
  [make-context       (sort-graph? signature? rulelist?
                       . -> . context?)]
  [check-regularity   (context? . -> . void?)]
  [context-sort-graph (context? . -> . sort-graph?)]
  [context-signature  (context? . -> . signature?)]
  [context-rules      (context? . -> . rulelist?)]
  [merge-contexts     (context? context? . -> . context?)]
  [rules-by-label     (context? symbol? . -> . list?)]
  [rule-by-label      (context? symbol? . -> . rule?)]))

(require "./sorts.rkt"
         "./operators.rkt"
         "./builtins.rkt"
         "./terms.rkt"
         (prefix-in ts: "./term-syntax.rkt")
         "./equations.rkt"
         "./condd.rkt"
         racket/stxparam
         threading
         (for-syntax (prefix-in ts: "./term-syntax.rkt")
                     syntax/parse
                     racket/stxparam))

(module+ test
  (require "./term-syntax.rkt"
           "./test-examples.rkt"
           rackunit
           racket/function))

;
; A context combines a sort-graph, a signature, and a rulelist.
;
(struct context (sort-graph signature rules)
        #:transparent
        #:methods gen:custom-write
        [(define (write-proc context port mode)
           (display "(context" port)
           (display-signature (context-signature context) 2 port)
           (display "\n  ; rules" port)
           (for ([rule (in-rules (context-rules context))])
             (display "\n  " port)
             (write rule port))
           (display ")\n" port))])

(define (make-context sort-graph signature rules)
  (context sort-graph signature rules))

(define (is-context? c)
  (context? c))

(define (valid-context? context)
  (condd
   [(not (context? context)) #f]
   #:do (define sorts (context-sort-graph context))
   #:do (define signature (context-signature context))
   [(not (equal? (signature-sort-graph signature) sorts)) #f]
   [else (for/and ([r (in-rules (context-rules context))])
           (valid-rule? signature r))]))

(define (check-regularity context)
  (define non-regular (non-regular-op-example
                          (context-signature context)))
  (when non-regular
    (error "signature not regular: " non-regular)))

(define empty-context
  (let ([sorts empty-sort-graph])
    (context sorts
             (empty-signature sorts)
             empty-rulelist)))

(module+ test
  (check-true (valid-context? empty-context)))

(define (merge-contexts context1 context2)
  (let* ([sorts (merge-sort-graphs (context-sort-graph context1)
                                   (context-sort-graph context2))]
         [signature (merge-signatures (context-signature context1)
                                      (context-signature context2)
                                      sorts)]
         [rules (merge-rulelists (context-rules context1)
                                 (context-rules context2)
                                 signature)])
    (context sorts signature rules)))

(module+ test

  (define basic-truth-context
    (context truth-sorts truth-signature empty-rulelist))

  (define basic-string-context
    (context string-sorts string-signature empty-rulelist))

  (define basic-integer-context
    (context integer-sorts integer-signature empty-rulelist))

  (define basic-rational-context
    (context rational-sorts rational-signature empty-rulelist))

  (check-equal? (merge-contexts basic-truth-context basic-string-context)
                (merge-contexts basic-string-context basic-truth-context))
  (check-equal? (merge-contexts basic-truth-context basic-truth-context)
                basic-truth-context)
  (check-equal? (merge-contexts empty-context basic-truth-context)
                basic-truth-context)
  (check-equal? (merge-contexts basic-truth-context empty-context)
                basic-truth-context)
  (check-equal? (merge-contexts basic-integer-context basic-rational-context)
                basic-rational-context)
  (check-equal? (foldl merge-contexts empty-context
                       (list basic-truth-context
                             basic-string-context
                             basic-rational-context))
                (foldl merge-contexts empty-context
                       (list basic-string-context
                             basic-rational-context
                             basic-truth-context)))
  (check-true (valid-context? (merge-contexts basic-string-context
                                              basic-rational-context)))
  (check-true (valid-context? (merge-contexts basic-truth-context
                                              basic-rational-context))))

;
; Syntax for context definitions
;
(begin-for-syntax

  (define-syntax-class include
    #:description "include declaration"
    (pattern ((~datum include) context:expr)))

  (define-syntax-class sort-or-subsort
    #:description "sort or subsort declaration"
    (pattern ((~datum sort) sort-name:id)
             #:with value
             #'(add-sort (quote sort-name)))
    (pattern ((~datum subsort) sort1:id sort2:id)
             #:with value
             #'(add-subsort-relation (quote sort1) (quote sort2))))

  (define-syntax-class operator
    #:description "operator declaration"
    (pattern ((~datum op) op-name:id sort:id)
             #:with value
             #'(add-op (quote op-name) empty (quote sort)))
    (pattern ((~datum op) (op-name:id arg-sort:id ...) sort:id)
             #:with value
             #'(add-op (quote op-name)
                       (list (quote arg-sort) ...)
                       (quote sort))))

  (define-syntax-class variable
    #:description "variable declaration"
    (pattern ((~datum var) var-name:id sort:id)
             #:with value
             #'(add-var (quote var-name) (quote sort))))

  ; Rules and equations

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
              a-label:opt-label
              local-vars:opt-vars
              (~var left (embedded-pattern sig vars))
              (~var right (embedded-pattern sig vars))
              (~var condition (opt-condition sig vars)))
             #:with expr #`(make-equation #,sig left.expr condition.expr
                                          right.expr a-label.expr)
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

(define-syntax (context- stx)
  (syntax-parse stx
    [(_ included-contexts:include ...
        sort-defs:sort-or-subsort ...
        op-defs:operator ...
        var-defs:variable ...
        (~var rule-defs (rule #'signature #'vars*)) ...)
     #`(let* ([sorts (~> (for/fold ([ms empty-sort-graph])
                                   ([s (list (context-sort-graph
                                              included-contexts.context) ...)])
                           (merge-sort-graphs ms s))
                         sort-defs.value ...)]
              [signature (~> (for/fold ([msig (empty-signature sorts)])
                                       ([sig (list (context-signature
                                                    included-contexts.context)
                                                   ...)])
                               (merge-signatures msig sig sorts))
                             op-defs.value ...
                             var-defs.value ...)]
              [rules (~> (for/fold ([mrl empty-rulelist])
                                   ([rl (list (context-rules
                                               included-contexts.context) ...)])
                           (merge-rulelists mrl rl signature))
                         (add-rule
                          (let ([vars* (local-vars (hash) rule-defs.vars)])
                            rule-defs.expr))
                         ...)])
         (context sorts signature rules))]))

(define-syntax (define-context stx)
  (syntax-parse stx
    [(_ name:id
        included-contexts:include ...
        sort-defs:sort-or-subsort ...
        op-defs:operator ...
        var-defs:variable ...
        (~var ruleq-defs (rule #'signature #'vars)) ...)
     #'(begin
         (define name
           (context- included-contexts ...
                     sort-defs ...
                     op-defs ...
                     var-defs ...
                     ruleq-defs ...))
         (check-regularity name))]))

(module+ test
  (define a-context
    (context-
     (include basic-truth-context)
     (include basic-rational-context)
     (sort A) (sort B)
     (subsort B A)
     (sort X) (sort Y)
     (subsort Y X)
     (op an-A A)
     (op a-B B)
     (op an-X X)
     (op a-Y Y)
     (op foo B)
     (op (foo B) A)
     (op (foo A B) A)
     (var Avar A)
     (var Bvar B)
     (var IntVar ℤ)
     (var BoolVar boolean)))

  (check-equal? (context-sort-graph a-context) sorts)
  (check-equal? (context-signature a-context) a-signature)
 
 (define-context test1
    (include basic-truth-context)
    (sort A) (sort B)
    (op an-A A)
    (op a-B B)
    (op (foo B) A)
    (op (foo A) B)
    (var X B)
    (=> #:label a-rule (foo an-A) a-B)
    (=> (foo X) an-A
        #:if true))

  (check-equal? (hash-keys (context-rules test1)) '(foo))
  (check-equal? (length (hash-ref (context-rules test1) 'foo)) 2 )

  (with-context test1
    (check-equal? (term.sort (T an-A)) 'A)))

;
; Lookup rules by label
;
(define (rules-by-label context label)
  (for/list ([rule (in-rules (context-rules context))]
             #:when (equal? label (rule-label rule)))
    rule))

(define (rule-by-label context label)
  (define all (rules-by-label context label))
  (unless (equal? (length all) 1)
    (if (empty? all)
        (error (format "No rule with label ~a" label))
        (error (format "More than one rule with label ~a" label))))
  (first all))

(module+ test
  (check-equal? (rules-by-label test1 'foo) empty))

;
; Definition of terms, rules, and equations inside with-context
;
(define-syntax-parameter eq
  (λ (stx)
    (raise-syntax-error 'eq "eq keyword used outside with-context" stx)))

(define-syntax-parameter tr
  (λ (stx)
    (raise-syntax-error 'tr "tr keyword used outside with-context" stx)))

(define-syntax (with-context stx)
  (syntax-parse stx
    [(_ c:expr body ...)
     #'(syntax-parameterize
           ([eq (λ (stx)
                  (syntax-parse stx
                    [(~var eqn (equation #'(context-signature c)
                                         #'vars*))
                     #'(let ([vars* (local-vars (hash) eqn.vars)])
                         eqn.expr)]))]
            [tr (λ (stx)
                  (syntax-parse stx
                    [(_  (~var pr (rule-hp #'(context-signature c)
                                           #'vars*)))
                     #'(let ([vars* (local-vars (hash) pr.vars)])
                         (make-transformation
                          (context-signature c)
                          (apply make-rule (append pr.args '(#f)))))]))])
         (ts:with-signature (context-signature c)
                            body ...))]))

(module+ test
  (with-context test1
    (check-equal? (set-count
                   (term.vars
                    (equation-left
                     (eq #:label an-equation #:var (Foo A) (foo Foo) a-B))))
                  1)
    (check-equal? (set-count
                   (term.vars
                    (rule-pattern
                     (transformation-converted-rule
                      (tr #:label a-transformation #:var (Foo A)
                          (foo Foo) a-B)))))
                  1)))

