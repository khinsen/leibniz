#lang racket

(provide 
 (rename-out [context builtin-context]
             [context- context])
 define-context
 with-context eq tr
 (contract-out
  [context?             (any/c . -> . boolean?)]
  [context-signature    (context? . -> . signature?)]
  [context-rules        (context? . -> . rulelist?)]
  [context-vars         (context? . -> . varset?)]
  [rules-by-label       (context? symbol? . -> . list?)]
  [rule-by-label        (context? symbol? . -> . rule?)]
  [equations-by-label   (context? symbol? . -> . set?)]
  [equation-by-label   (context? symbol? . -> . equation?)]))

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
; A context combines a sort-graph, a signature, a varset, a rulelist,
; and an equationset.
;
(struct context (sort-graph signature vars rules equations)
        #:transparent
        #:methods gen:custom-write
        [(define (write-proc context port mode)
           (display "(context" port)
           (display-signature (context-signature context) 2 port)
           (display "\n  ; variables" port)
           (for ([(symbol sort) (all-vars (context-vars context))])
             (display (format "\n  (var ~s ~s)" symbol sort) port))
           (display "\n  ; rules" port)
           (for ([rule (in-rules (context-rules context))])
             (display "\n  " port)
             (write rule port))
           (display "\n  ; equations" port)
           (for ([eq (in-equations (context-equations context))])
             (display "\n  " port)
             (write eq port))
           (display ")\n" port))])

(define (valid-context? context)
  (condd
   [(not (context? context)) #f]
   #:do (define sorts (context-sort-graph context))
   #:do (define signature (context-signature context))
   [(not (equal? (signature-sort-graph signature) sorts)) #f]
   [(not (equal? (varset-sort-graph (context-vars context)) sorts)) #f]
   [else (and (for/and ([r (in-rules (context-rules context))])
                (valid-rule? signature r))
              (for/and ([e (in-equations (context-equations context))])
                (valid-equation? signature e)))]))

(define empty-context
  (let ([sorts empty-sort-graph])
    (context sorts
             (empty-signature sorts)
             (empty-varset sorts)
             empty-rulelist
             empty-equationset)))

(module+ test
  (check-true (valid-context? empty-context)))

(define (merge-contexts context1 context2)
  (let* ([sorts (merge-sort-graphs (context-sort-graph context1)
                                   (context-sort-graph context2))]
         [signature (merge-signatures (context-signature context1)
                                      (context-signature context2)
                                      sorts)]
         [vars (merge-varsets (context-vars context1)
                              (context-vars context2)
                              sorts)]
         [rules (merge-rulelists (context-rules context1)
                                 (context-rules context2)
                                 signature)]
         [equations (merge-equationsets (context-equations context1)
                                        (context-equations context2)
                                        signature)])
    (context sorts signature vars rules equations)))

(module+ test

  (define basic-truth-context
    (context truth-sorts truth-signature
             (empty-varset truth-sorts) empty-rulelist empty-equationset))

  (define basic-string-context
    (context string-sorts string-signature
             (empty-varset string-sorts) empty-rulelist empty-equationset))

  (define basic-integer-context
    (context integer-sorts integer-signature
             (empty-varset integer-sorts) empty-rulelist empty-equationset))

  (define basic-rational-context
    (context rational-sorts rational-signature
             (empty-varset rational-sorts) empty-rulelist empty-equationset))

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
    #:description "optional label in a rule or equation"
    (pattern (~seq #:label a-symbol:id)
             #:with expr #'(quote a-symbol))
    (pattern (~seq)
             #:with expr #'#f))

  (define-splicing-syntax-class opt-vars
    #:description "optional variable declaration in a rule or equation"
    (pattern (~seq #:vars ([var-name:id var-sort:id] ...))
             #:with expr #'(list (cons (quote var-name)
                                       (quote var-sort)) ...))
    (pattern (~seq #:var [var-name:id var-sort:id])
             #:with expr #'(list (cons (quote var-name)
                                       (quote var-sort))))
    (pattern (~seq)
             #:with expr #'empty))

  (define-splicing-syntax-class (embedded-pattern sig varset)
    #:description "embedded pattern in a rule or equation"
    (pattern p:expr
             #:with expr #`(ts:pattern #,sig #,varset p)))

  (define-splicing-syntax-class (opt-condition sig varset)
    #:description "optional condition in a rule or equation"
    (pattern (~seq #:if (~var condition (embedded-pattern sig varset)))
             #:with expr #'condition.expr)
    (pattern (~seq)
             #:with expr #'#f))

  (define-splicing-syntax-class (rule-hp sig varset)
    (pattern  (~seq a-label:opt-label
                    local-vars:opt-vars
                    (~var pattern (embedded-pattern sig varset))
                    (~var replacement (embedded-pattern sig varset))
                    (~var condition (opt-condition sig varset)))
              #:with args #`(list #,sig pattern.expr condition.expr
                                  replacement.expr a-label.expr)
              #:with vars #'local-vars.expr))

  (define-syntax-class (pattern-rule sig varset)
    #:description "pattern rule declaration"
    (pattern ((~datum =>)
              (~var prhp (rule-hp sig varset)))
             #:with expr #`(apply make-rule (append prhp.args '(#t)))
             #:with vars #'prhp.vars))

  (define-syntax-class (fn-rule sig varset)
    #:description "function rule declaration"
    (pattern ((~datum ->)
              a-label:opt-label
              local-vars:opt-vars
              (~var pattern (embedded-pattern sig varset))
              replacement:expr
              (~var condition (opt-condition sig varset)))
             #:with expr #`(make-rule #,sig pattern.expr condition.expr
                                      replacement a-label.expr #f)
             #:with vars #'local-vars.expr))

  (define-syntax-class (equation sig varset)
    #:description "equation declaration"
    (pattern ((~datum eq)
              a-label:opt-label
              local-vars:opt-vars
              (~var left (embedded-pattern sig varset))
              (~var right (embedded-pattern sig varset))
              (~var condition (opt-condition sig varset)))
             #:with expr #`(make-equation #,sig left.expr condition.expr
                                          right.expr a-label.expr)
             #:with vars #'local-vars.expr))

  (define-syntax-class (rule-or-eq sig varset)
    #:description "rule or equation declaration"
    (pattern (~var pr (pattern-rule sig varset))
             #:with expr #'pr.expr
             #:with vars #'pr.vars)
    (pattern (~var fr (fn-rule sig varset))
             #:with expr #'fr.expr
             #:with vars #'fr.vars)
    (pattern (~var e (equation sig varset))
             #:with expr #'e.expr
             #:with vars #'e.vars)))

(define (add-vars varset var-defs)
  (foldl (λ (vd vs) (add-var vs (car vd) (cdr vd))) varset var-defs))

(define (add-rule-or-eq ruleqs rule-or-eq)
  (if (rule? rule-or-eq)
      (cons (add-rule (car ruleqs) rule-or-eq)
                (cdr ruleqs))
      (cons (car ruleqs)
            (add-equation (cdr ruleqs) rule-or-eq))))

(define-syntax (context- stx)
  (syntax-parse stx
    [(_ included-contexts:include ...
        sort-defs:sort-or-subsort ...
        op-defs:operator ...
        var-defs:variable ...
        (~var ruleq-defs (rule-or-eq #'signature #'varset*)) ...)
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
                             op-defs.value ...)]
              [varset (~> (for/fold ([mv (empty-varset sorts)])
                                    ([v (list (context-vars
                                               included-contexts.context) ...)])
                            (merge-varsets mv v sorts))
                          var-defs.value ...)]
              [rules (for/fold ([mrl empty-rulelist])
                               ([rl (list (context-rules
                                           included-contexts.context) ...)])
                       (merge-rulelists mrl rl signature))]
              [equations (for/fold ([mes empty-equationset])
                                   ([es (list (context-equations
                                               included-contexts.context) ...)])
                           (merge-equationsets mes es signature))]
              [ruleqs (~> (cons rules equations)
                          (add-rule-or-eq
                           (let ([varset* (add-vars varset ruleq-defs.vars)])
                             ruleq-defs.expr))
                          ...)])
         (context sorts signature varset (car ruleqs) (cdr ruleqs)))]))

(define-syntax (define-context stx)
  (syntax-parse stx
    [(_ name:id
        included-contexts:include ...
        sort-defs:sort-or-subsort ...
        op-defs:operator ...
        var-defs:variable ...
        (~var ruleq-defs (rule-or-eq #'signature #'varset)) ...)
     #'(define name
         (context- included-contexts ...
                   sort-defs ...
                   op-defs ...
                   var-defs ...
                   ruleq-defs ...))]))

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
     (var IntVar Integer)
     (var BoolVar Boolean)))

  (check-equal? (context-sort-graph a-context) sorts)
  (check-equal? (context-signature a-context) a-signature)
  (check-equal? (context-vars a-context) a-varset)
 
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
        #:if true)
    (eq #:label an-equation an-A (foo a-B)))

  (check-equal? (hash-keys (context-rules test1)) '(foo))
  (check-equal? (length (hash-ref (context-rules test1) 'foo)) 2 )

  (with-context test1
    (check-equal? (term.sort (T an-A)) 'A))

  (define-context test2
    (include basic-truth-context)
    (sort A) (sort B)
    (op an-A A)
    (op a-B B)
    (op (foo B) A)
    (op (foo A) B)
    (=> #:label a-rule (foo an-A) a-B)
    (=> #:var [X B]
        (foo X) an-A
        #:if true)
    (eq #:label an-equation an-A (foo a-B)))

  (check-equal? (context-sort-graph test1) (context-sort-graph test2))
  (check-equal? (context-signature test1) (context-signature test2))
  ; context-vars must be different
  (check-equal? (context-rules test1) (context-rules test2))
  (check-equal? (context-equations test1) (context-equations test2)))

;
; Lookup rules and equations by label
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

(define (equations-by-label context label)
  (for/set ([equation (in-equations (context-equations context))]
             #:when (equal? label (equation-label equation)))
    equation))

(define (equation-by-label context label)
  (define all (equations-by-label context label))
  (unless (equal? (set-count all) 1)
    (if (set-empty? all)
        (error (format "No equation with label ~a" label))
        (error (format "More than one equation with label ~a" label))))
  (set-first all))

(module+ test
  (check-equal? (equations-by-label test1 'foo) (set))
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
                                         #'varset*))
                     #'(let ([varset* (add-vars (context-vars c) eqn.vars)])
                         eqn.expr)]
                    [(_ label:id)
                     #'(equation-by-label c (quote label))]))]
            [tr (λ (stx)
                  (syntax-parse stx
                    [(_  (~var pr (rule-hp #'(context-signature c)
                                           #'varset*)))
                     #'(let ([varset* (add-vars (context-vars c) pr.vars)])
                         (make-transformation
                          (context-signature c)
                          (apply make-rule (append pr.args '(#f)))))]))])
         (ts:with-sig-and-vars (context-signature c) (context-vars c)
                               body ...))]))

(module+ test
  (with-context test1
    (check-equal? (eq an-equation)
                  (eq #:label an-equation an-A (foo a-B)))
    (check-equal? (set-count
                   (term.vars
                    (equation-left
                     (eq #:label another-equation #:var (Foo A) (foo Foo) a-B))))
                  1)
    (check-equal? (set-count
                   (term.vars
                    (rule-pattern
                     (transformation-converted-rule
                      (tr #:label a-transformation #:var (Foo A)
                          (foo Foo) a-B)))))
                  1)))

