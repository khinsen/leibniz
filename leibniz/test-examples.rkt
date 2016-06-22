#lang racket

(provide sorts a-signature a-varset)

(require "./sorts.rkt"
         "./operators.rkt"
         "./builtins.rkt"
         "./terms.rkt"
         rackjure/threading)

(define sorts
  (~> (merge-sort-graphs exact-number-sorts truth-sorts)
      (add-sort 'A) (add-sort 'B)
      (add-subsort-relation 'B 'A)
      (add-sort 'X) (add-sort 'Y)
      (add-subsort-relation 'Y 'X)))

(define a-signature
  (~>  (foldl merge-signatures (empty-signature sorts)
              (list exact-number-signature truth-signature))
      (add-op 'an-A empty 'A)
      (add-op 'a-B empty 'B)
      (add-op 'an-X empty 'X)
      (add-op 'a-Y empty 'Y)
      (add-op 'foo empty 'B)
      (add-op 'foo (list 'B) 'A)
      (add-op 'foo (list 'A 'B) 'A)))

(define a-varset
  (~> (empty-varset sorts)
      (add-var 'Avar 'A)
      (add-var 'Bvar 'B)
      (add-var 'IntVar 'Integer)
      (add-var 'BoolVar 'Boolean)))
