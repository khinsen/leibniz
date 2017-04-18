#lang racket

(provide sorts a-signature)

(require "./sorts.rkt"
         "./operators.rkt"
         "./builtins.rkt"
         "./terms.rkt"
         threading)

(define sorts
  (~> (merge-sort-graphs rational-sorts truth-sorts)
      (add-sort 'A) (add-sort 'B)
      (add-subsort-relation 'B 'A)
      (add-sort 'X) (add-sort 'Y)
      (add-subsort-relation 'Y 'X)))

(define a-signature
  (~>  (foldl (λ (s1 s2) (merge-signatures s1 s2 #f))
              (empty-signature sorts)
              (list rational-signature truth-signature))
      (add-op 'an-A empty 'A)
      (add-op 'a-B empty 'B)
      (add-op 'an-X empty 'X)
      (add-op 'a-Y empty 'Y)
      (add-op 'foo empty 'B)
      (add-op 'foo (list 'B) 'A)
      (add-op 'foo (list 'A 'B) 'A)
      (add-var 'Avar 'A)
      (add-var 'Bvar 'B)
      (add-var 'IntVar 'ℤ)
      (add-var 'BoolVar 'boolean)))

