#lang racket

(provide
 (contract-out
  [boolean context?]
  [integers context?]
  [exact-numbers context?]))

(require "./builtins.rkt"
         "./terms.rkt"
         "./context-syntax.rkt"
         (only-in "./contexts.rkt" context? truth-context
                                   integer-context exact-number-context))

(module+ test
  (require chk))

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

;
; Integers and exact numbers
;

(define (binary-op predicate proc)
  (λ (signature pattern condition substitution)
    (let ([x (substitution-value substitution 'X)]
          [y (substitution-value substitution 'Y)])
      (unless (and (predicate x) (predicate y))
        (error "arguments not numbers"))
      (proc x y))))

(define-context integers
  (include integer-context)
  (-> #:vars ([X Integer] [Y Integer])
      (+ X Y) (binary-op exact? +))
  (-> #:vars ([X Integer] [Y Integer])
      (- X Y) (binary-op exact? -))
  (-> #:vars ([X Integer] [Y Integer])
      (* X Y) (binary-op exact? *)))

(module+ test
  (with-context integers
    (chk
     #:= (RT (+ 2 3)) (T 5)
     #:= (RT (- 2 3)) (T -1)
     #:= (RT (* 2 3)) (T 6))))

(define-context exact-numbers
  (include exact-number-context)
  (-> #:vars ([X Rational] [Y Rational])
      (+ X Y) (binary-op exact? +))
  (-> #:vars ([X Rational] [Y Rational])
      (- X Y) (binary-op exact? -))
  (-> #:vars ([X Rational] [Y Rational])
      (* X Y) (binary-op exact? *))
  (-> #:vars ([X Rational] [Y Rational])
    (/ X Y) (binary-op exact? /)))

(module+ test
  (with-context exact-numbers
    (chk
     #:= (RT (+ 2 3)) (T 5)
     #:= (RT (+ 1/2 -1/2)) (T 0)
     #:= (RT (- 2 3)) (T -1)
     #:= (RT (- 1/2 1/2)) (T 0)
     #:= (RT (* 2 3)) (T 6)
     #:= (RT (* 1/2 1/2)) (T 1/4)
     #:= (RT (/ 2 3)) (T 2/3)
     #:= (RT (/ 1/2 1/2)) (T 1)
     ; Division by zero throws and exception, so no rewrite
     #:= (RT (/ 1 0)) (T (/ 1 0)))))
