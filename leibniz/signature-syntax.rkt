#lang racket

(provide signature)

(require "./sorts.rkt"
         "./operators.rkt"
         threading
         racket/stxparam
         (for-syntax syntax/parse
                     racket/stxparam))

(module+ test
  (require rackunit
           "./builtins.rkt"
           "./test-examples.rkt"))

(begin-for-syntax

  (define-syntax-class include
    #:description "include declaration"
    (pattern ((~datum include) signature:expr)))

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
             #'(add-var (quote var-name) (quote sort)))))

(define-syntax (signature stx)
  (syntax-parse stx
    [(_ inclusions:include ...
        sort-defs:sort-or-subsort ...
        op-defs:operator ...
        var-defs:variable ...)
     #`(let ([sorts (~> (for/fold ([ms empty-sort-graph])
                                  ([s (list (signature-sort-graph
                                             inclusions.signature) ...)])
                          (merge-sort-graphs ms s))
                        sort-defs.value ...)])
         (~> (for/fold ([msig (empty-signature sorts)])
                       ([sig (list inclusions.signature ...)])
               (merge-signatures msig sig sorts))
             op-defs.value ...
             var-defs.value ...))]))

(module+ test
  (define sig
    (signature
     (include truth-signature)
     (include rational-signature)
     (sort A)
     (sort B)
     (subsort B A)
     (sort X)
     (sort Y)
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
  (check-equal? sig a-signature))
