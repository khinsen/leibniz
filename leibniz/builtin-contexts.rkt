#lang racket

(provide
 (contract-out
  [boolean context?]))

(require "./builtins.rkt"
         "./terms.rkt"
         "./context-syntax.rkt"
         (only-in "./contexts.rkt" context? truth-context))

(module+ test
  (require chk))

;
; Syntactic equality of terms
;
(define-context equality
  (include truth-context)
  (op (== [*] [*]) Boolean)
  (fn (== x y) (λ (signature op args)
                 (make-term signature
                            (if (equal? (first args) (second args)) 'true 'false)
                            empty))))

(module+ test
  (with-context equality
    (chk
     #:= (RT (== true true))  (T true)
     #:= (RT (== true false)) (T false))))

;
; Boolean algebra
;
; Adapted from
; http://maude.cs.uiuc.edu/maude1/manual/maude-manual-html/maude-manual_16.html
;
(define-context boolean
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
  (with-context boolean
    (chk
     #:= (RT (not true))        (T false)
     #:= (RT (not false))       (T true)
     #:= (RT (and true true))   (T true)
     #:= (RT (and true false))  (T false)
     #:= (RT (and false true))  (T false)
     #:= (RT (and false false)) (T false)
     #:= (RT (or true true))    (T true)
     #:= (RT (or true false))   (T true)
     #:= (RT (or false true))   (T true)
     #:= (RT (or false false))  (T false)
     #:= (RT (xor true true))   (T false)
     #:= (RT (xor true false))  (T true)
     #:= (RT (xor false true))  (T true)
     #:= (RT (xor false false)) (T false)
     #:= (RT (=> true true))    (T true)
     #:= (RT (=> true false))   (T false)
     #:= (RT (=> false true))   (T true)
     #:= (RT (=> false false))  (T true))))
