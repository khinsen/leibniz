#lang racket

(provide 
 (rename-out [context- context])
 define-context
 with-context
 (contract-out
  [context?             (any/c . -> . boolean?)]
  [context-signature    (context? . -> . signature?)]
  [context-rules        (context? . -> . rulelist?)]
  [truth-context        context?]
  [symbol-context       context?]
  [string-context       context?]
  [integer-context      context?]
  [exact-number-context context?]))

(require "./sorts.rkt"
         "./operators.rkt"
         "./builtins.rkt"
         "./terms.rkt"
         (prefix-in ts: "./term-syntax.rkt")
         "./equations.rkt"
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
(struct context (sort-graph signature vars rules)
        #:transparent)

(define empty-context
  (let ([sorts empty-sort-graph])
    (context sorts
             (empty-signature sorts)
             (empty-varset sorts)
             empty-rulelist)))

(define (merge-contexts context1 context2)
  (let* ([signature (merge-signatures (context-signature context1)
                                      (context-signature context2))]
         [vars (merge-varsets (context-vars context1)
                              (context-vars context2))]
         [sorts (signature-sort-graph signature)]
         [rules empty-rulelist])
    (context sorts signature vars rules)))

;
; Builtin contexts
;
(define truth-context
  (context truth-sorts truth-signature
           (empty-varset truth-sorts) empty-rulelist))

(define symbol-context
  (context symbol-sorts symbol-signature
           (empty-varset symbol-sorts) empty-rulelist))

(define string-context
  (context string-sorts string-signature
           (empty-varset string-sorts) empty-rulelist))

(define integer-context
  (context integer-sorts integer-signature
           (empty-varset integer-sorts) empty-rulelist))

(define exact-number-context
  (context exact-number-sorts exact-number-signature
           (empty-varset exact-number-sorts) empty-rulelist))

(module+ test
  (check-equal? (merge-contexts truth-context symbol-context)
                (merge-contexts symbol-context truth-context))
  (check-equal? (merge-contexts truth-context truth-context)
                truth-context)
  (check-equal? (merge-contexts empty-context truth-context)
                truth-context)
  (check-equal? (merge-contexts truth-context empty-context)
                truth-context)
  (check-equal? (merge-contexts integer-context exact-number-context)
                exact-number-context)
  (check-equal? (foldl merge-contexts empty-context
                       (list truth-context string-context exact-number-context))
                (foldl merge-contexts empty-context
                       (list string-context exact-number-context truth-context)))
)
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

  (define-syntax-class rule
    #:description "rule declaration"
    (pattern ((~literal =>) pattern replacement
              (~optional (~seq #:if condition)))
             #:with p-term #'(ts:T pattern)
             #:with r-term #'(ts:T replacement)
             #:with c-term (if (attribute condition)
                               #'(ts:T condition)
                               #'#f))))

(define-syntax (context- stx)
  (syntax-parse stx
    [(_ included-contexts:include ...
        sort-defs:sort-or-subsort ...
        op-defs:operator ...
        var-defs:variable ...
        rule-defs:rule ...)
     #'(let* ([initial (foldl merge-contexts empty-context
                              (list included-contexts.context ...))]
              [sorts (~> (context-sort-graph initial) sort-defs.value ...)]
              [signature (~> (merge-signatures (empty-signature sorts)
                                               (context-signature initial))
                             op-defs.value ...)]
              [varset (~> (merge-varsets (empty-varset sorts)
                                         (context-vars initial))
                          var-defs.value ...)]
              [rules (ts:with-sig-and-vars signature varset
                       (~> empty-rulelist
                           (add-rule (make-rule signature
                                                rule-defs.p-term
                                                rule-defs.c-term
                                                rule-defs.r-term))
                           ...))])
         (context sorts signature varset rules))]))

(define-syntax (define-context stx)
  (syntax-parse stx
    [(_ name:id
        included-contexts:include ...
        sort-defs:sort-or-subsort ...
        op-defs:operator ...
        var-defs:variable ...
        rule-defs:rule ...)
     #'(define name
         (context- included-contexts ...
                   sort-defs ...
                   op-defs ...
                   var-defs ...
                   rule-defs ...))]))

(define-syntax (with-context stx)
  (syntax-parse stx
    [(_ c:expr body ...)
     #'(ts:with-sig-and-vars (context-signature c) (context-vars c)
         body ...)]))

(module+ test
  (define a-context
    (context-
     (include truth-context)
     (include exact-number-context)
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

  (define-context test
    (include truth-context)
    (sort A) (sort B)
    (op an-A A)
    (op a-B B)
    (op (foo B) A)
    (op (foo A) B)
    (var X B)
    (=> (foo an-A) a-B)
    (=> (foo X) an-A
        #:if true))

  (check-equal? (hash-keys (context-rules test)) '(foo))
  (check-equal? (length (hash-ref (context-rules test) 'foo)) 2 )

  (with-context test
    (check-equal? (term.sort (T an-A)) 'A)))

