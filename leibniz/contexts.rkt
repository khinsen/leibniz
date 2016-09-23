#lang racket

(provide 
 (rename-out [context builtin-context]
             [context- context])
 define-context
 with-context
 (contract-out
  [context?             (any/c . -> . boolean?)]
  [context-signature    (context? . -> . signature?)]
  [context-rules        (context? . -> . rulelist?)]))

(require "./sorts.rkt"
         "./operators.rkt"
         "./builtins.rkt"
         "./terms.rkt"
         (prefix-in ts: "./term-syntax.rkt")
         "./equations.rkt"
         "./condd.rkt"
         rackjure/threading
         racket/stxparam
         (for-syntax (prefix-in ts: "./term-syntax.rkt")
                     syntax/parse
                     racket/stxparam))

(module+ test
  (require "./term-syntax.rkt"
           "./test-examples.rkt"
           rackunit
           racket/function
           rackjure/threading))

;
; A context combines a sort-graph, a signature, a varset, and a rulelist.
; (Equations to be added later)
;
(struct context (sort-graph signature vars rules equations)
        #:transparent
        #:methods gen:custom-write
        [(define (write-proc context port mode)
           (write-string "(context" port)
           (write-signature (context-signature context) 2 port)
           (write-string "\n  ; rules" port)
           (for ([rule (in-rules (context-rules context))])
             (newline port)
             (write rule port))
           (write-string "\n  ; equations" port)
           (for ([eq (in-equations (context-equations context))])
             (newline port)
             (write eq port))
           (write-string ")\n" port))])

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
  (let* ([signature (merge-signatures (context-signature context1)
                                      (context-signature context2))]
         [vars (merge-varsets (context-vars context1)
                              (context-vars context2))]
         [sorts (signature-sort-graph signature)]
         [rules (merge-rulelists signature
                                 (context-rules context1)
                                 (context-rules context2))]
         [equations (merge-equationsets signature
                                        (context-equations context1)
                                        (context-equations context2))])
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
    (pattern ((~literal include) context:expr)))

  (define-syntax-class sort-or-subsort
    #:description "sort or subsort declaration"
    (pattern ((~literal sort) sort-name:id)
             #:with value
             #'(add-sort (quote sort-name)))
    (pattern ((~literal subsort) sort1:id sort2:id)
             #:with value
             #'(add-subsort-relation (quote sort1) (quote sort2))))

  (define-syntax-class operator
    #:description "operator declaration"
    (pattern ((~literal op) op-name:id sort:id)
             #:with value
             #'(add-op (quote op-name) empty (quote sort)))
    (pattern ((~literal op) (op-name:id arg-sort:id ...) sort:id)
             #:with value
             #'(add-op (quote op-name)
                       (list (quote arg-sort) ...)
                       (quote sort))))

  (define-syntax-class variable
    #:description "variable declaration"
    (pattern ((~literal var) var-name:id sort:id)
             #:with value
             #'(add-var (quote var-name) (quote sort))))

  (define-syntax-class (rule-or-eq sig-var vars-var)
    #:description "rule or equation declaration"
    (pattern ((~literal =>)
              (~optional (~seq #:vars ([var-name:id var-sort:id] ...)))
              (~optional (~seq #:label label-symbol:id))
              pattern replacement
              (~optional (~seq #:if condition)))
             #:with type #'(quote rule)
             #:with vars (if (attribute var-name)
                             #'(list (cons (quote var-name)
                                           (quote var-sort)) ...)
                             #'empty)
             #:with label (if (attribute label-symbol)
                              #'(quote label-symbol)
                              #'#f)
             #:with l-term #`(ts:pattern #,sig-var #,vars-var pattern)
             #:with r-term #`(ts:pattern #,sig-var #,vars-var replacement)
             #:with c-term (if (attribute condition)
                               #`(ts:pattern #,sig-var #,vars-var condition)
                               #'#f))
    (pattern ((~literal ->)
              (~optional (~seq #:vars ([var-name:id var-sort:id] ...)))
              (~optional (~seq #:label label-symbol:id))
              pattern replacement:expr
              (~optional (~seq #:if condition)))
             #:with type #'(quote rule)
             #:with vars (if (attribute var-name)
                             #'(list (cons (quote var-name)
                                           (quote var-sort)) ...)
                             #'empty)
             #:with label (if (attribute label-symbol)
                              #'(quote label-symbol)
                              #'#f)
             #:with l-term #`(ts:pattern #,sig-var #,vars-var pattern)
             #:with r-term #'replacement
             #:with c-term (if (attribute condition)
                               #`(ts:pattern #,sig-var #,vars-var condition)
                               #'#f))
    (pattern ((~literal eq)
              (~optional (~seq #:vars ([var-name:id var-sort:id] ...)))
              (~optional (~seq #:label label-symbol:id))
              left right
              (~optional (~seq #:if condition)))
             #:with type #'(quote equation)
             #:with vars (if (attribute var-name)
                             #'(list (cons (quote var-name)
                                           (quote var-sort)) ...)
                             #'empty)
             #:with label (if (attribute label-symbol)
                              #'(quote label-symbol)
                              #'#f)
             #:with l-term #`(ts:pattern #,sig-var #,vars-var left)
             #:with r-term #`(ts:pattern #,sig-var #,vars-var right)
             #:with c-term (if (attribute condition)
                               #`(ts:pattern #,sig-var #,vars-var condition)
                               #'#f))))

(define (add-vars* varset var-defs)
  (foldl (λ (vd vs) (add-var vs (car vd) (cdr vd))) varset var-defs))

(define (add-rule-or-eq ruleqs type rule-or-eq)
  (case type
    [(rule) (cons (add-rule (car ruleqs) rule-or-eq)
                  (cdr ruleqs))]
    [(equation) (cons (car ruleqs)
                      (add-equation (cdr ruleqs) rule-or-eq))]))

(define (make-rule-or-eq signature type l-term c-term r-term label)
  (case type
    [(rule) (make-rule signature l-term c-term r-term label)]
    [(equation) (make-equation signature l-term c-term r-term label)]))

(define-syntax (context- stx)
  (syntax-parse stx
    [(_ included-contexts:include ...
        sort-defs:sort-or-subsort ...
        op-defs:operator ...
        var-defs:variable ...
        (~var ruleq-defs (rule-or-eq #'signature #'r-varset)) ...)
     #`(let* ([initial (foldl merge-contexts empty-context
                              (list included-contexts.context ...))]
              [sorts (~> (context-sort-graph initial) sort-defs.value ...)]
              [signature (~> (merge-signatures (empty-signature sorts)
                                               (context-signature initial))
                             op-defs.value ...)]
              [varset (~> (merge-varsets (empty-varset sorts)
                                         (context-vars initial))
                          var-defs.value ...)]
              [ruleqs (~> (cons (context-rules initial)
                                (context-equations initial))
                          (add-rule-or-eq ruleq-defs.type
                           (let ([r-varset (add-vars* varset ruleq-defs.vars)])
                             (make-rule-or-eq signature
                                              ruleq-defs.type
                                              ruleq-defs.l-term
                                              ruleq-defs.c-term
                                              ruleq-defs.r-term
                                              ruleq-defs.label)))
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

(define-syntax (with-context stx)
  (syntax-parse stx
    [(_ c:expr body ...)
     #'(ts:with-sig-and-vars (context-signature c) (context-vars c)
         body ...)]))

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
    (=> #:vars ([X B])
        (foo X) an-A
        #:if true)
    (eq #:label an-equation an-A (foo a-B)))

  (check-equal? (context-sort-graph test1) (context-sort-graph test2))
  (check-equal? (context-signature test1) (context-signature test2))
  ; context-vars must be different
  (check-equal? (context-rules test1) (context-rules test2)))
