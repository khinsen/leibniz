#lang racket

(provide
 (contract-out
  [boolean-context context?]))

(require "./builtins.rkt"
         "./terms.rkt"
         "./contexts.rkt")

(module+ test
  (require "./term-syntax.rkt")
  (require "./rewrite.rkt")
  (require chk))

;
; Boolean algebra
;
; Adapted from
; http://maude.cs.uiuc.edu/maude1/manual/maude-manual-html/maude-manual_16.html
;
(define-context boolean-context
  (include truth-context)
  (op (not Boolean) Boolean)
  (op (and Boolean Boolean) Boolean)
  (op (or Boolean Boolean) Boolean)
  (op (xor Boolean Boolean) Boolean)
  (op (=> Boolean Boolean) Boolean)

  (=> #:vars ([X Boolean])
      (not X) (xor true X))

  (=> #:vars ([X Boolean])
      (and false X) false)
  (=> #:vars ([X Boolean])
      (and X false) false)
  (=> #:vars ([X Boolean])
      (and true X) X)
  (=> #:vars ([X Boolean])
      (and X true) X)
  (=> #:vars ([X Boolean])
      (and X X) X)
  (=> #:vars ([X Boolean] [Y Boolean] [Z Boolean])
      (and X (xor Y Z)) (xor (and X Y) (and X Z)))

  (=> #:vars ([X Boolean] [Y Boolean])
      (or X Y) (xor X (xor Y (and X Y))))

  (=> #:vars ([X Boolean])
      (xor false X) X)
  (=> #:vars ([X Boolean])
      (xor X false) X)
  (=> #:vars ([X Boolean])
      (xor X X) false)

  (=> #:vars ([X Boolean] [Y Boolean])
      (=> X Y) (not (xor X (and X Y)))))

(module+ test
  (with-context boolean-context
    (chk
     #:= (reduce boolean-context (T (not true)))        (T false)
     #:= (reduce boolean-context (T (not false)))       (T true)
     #:= (reduce boolean-context (T (and true true)))   (T true)
     #:= (reduce boolean-context (T (and true false)))  (T false)
     #:= (reduce boolean-context (T (and false true)))  (T false)
     #:= (reduce boolean-context (T (and false false))) (T false)
     #:= (reduce boolean-context (T (or true true)))    (T true)
     #:= (reduce boolean-context (T (or true false)))   (T true)
     #:= (reduce boolean-context (T (or false true)))   (T true)
     #:= (reduce boolean-context (T (or false false)))  (T false)
     #:= (reduce boolean-context (T (xor true true)))   (T false)
     #:= (reduce boolean-context (T (xor true false)))  (T true)
     #:= (reduce boolean-context (T (xor false true)))  (T true)
     #:= (reduce boolean-context (T (xor false false))) (T false)
     #:= (reduce boolean-context (T (=> true true)))    (T true)
     #:= (reduce boolean-context (T (=> true false)))   (T false)
     #:= (reduce boolean-context (T (=> false true)))   (T true)
     #:= (reduce boolean-context (T (=> false false)))  (T true))))
