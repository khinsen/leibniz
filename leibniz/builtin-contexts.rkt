#lang racket

(provide
 (contract-out
  [boolean context?]
  [integers context?]
  [exact-numbers context?]
  [IEEE-floating-point context?]))

(require "./builtins.rkt"
         "./terms.rkt"
         "./context-syntax.rkt"
         (only-in "./contexts.rkt" context? truth-context
                                   integer-context exact-number-context
                                   IEEE-float-context))

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

(define (comparison-op predicate proc)
  (λ (signature pattern condition substitution)
    (let ([x (substitution-value substitution 'X)]
          [y (substitution-value substitution 'Y)])
      (unless (and (predicate x) (predicate y))
        (error "arguments not numbers"))
      (make-term signature
                 (if (proc x y) 'true 'false)
                 empty))))

(define-context integers
  (include integer-context)
  (-> #:vars ([X Integer] [Y Integer])
      (+ X Y) (binary-op integer? +))
  (-> #:vars ([X Integer] [Y Integer])
      (- X Y) (binary-op integer? -))
  (-> #:vars ([X Integer] [Y Integer])
      (* X Y) (binary-op integer? *))
  (-> #:vars ([X Integer] [Y Integer])
      (div X Y) (binary-op integer? quotient))
  (-> #:vars ([X Integer] [Y Integer])
      (rem X Y) (binary-op integer? remainder))
  (-> #:vars ([X Integer] [Y Integer])
      (< X Y) (comparison-op integer? <))
  (-> #:vars ([X Integer] [Y Integer])
      (> X Y) (comparison-op integer? >))
  (-> #:vars ([X Integer] [Y Integer])
      (<= X Y) (comparison-op integer? <=))
  (-> #:vars ([X Integer] [Y Integer])
      (>= X Y) (comparison-op integer? >=)))

(module+ test
  (with-context integers
    (chk
     #:= (RT (+ 2 3)) (T 5)
     #:= (RT (- 2 3)) (T -1)
     #:= (RT (* 2 3)) (T 6)
     #:= (RT (div 2 3)) (T 0)
     #:= (RT (rem 2 3)) (T 2)
     #:= (RT (< 2 3)) (T true)
     #:= (RT (> 2 3)) (T false)
     #:= (RT (<= 2 3)) (T true)
     #:= (RT (>= 2 3)) (T false)
     #:= (RT (<= 3 3)) (T true)
     #:= (RT (>= 3 3)) (T true))))

(define-context exact-numbers
  (include exact-number-context)
  (-> #:vars ([X Rational] [Y Rational])
      (+ X Y) (binary-op exact? +))
  (-> #:vars ([X Rational] [Y Rational])
      (- X Y) (binary-op exact? -))
  (-> #:vars ([X Rational] [Y Rational])
      (* X Y) (binary-op exact? *))
  (-> #:vars ([X Rational] [Y Rational])
    (/ X Y) (binary-op exact? /))
  (-> #:vars ([X Rational] [Y Rational])
      (< X Y) (comparison-op exact? <))
  (-> #:vars ([X Rational] [Y Rational])
      (> X Y) (comparison-op exact? >))
  (-> #:vars ([X Rational] [Y Rational])
      (<= X Y) (comparison-op exact? <=))
  (-> #:vars ([X Rational] [Y Rational])
      (>= X Y) (comparison-op exact? >=))
  (-> #:vars ([X Integer] [Y Integer])
      (div X Y) (binary-op integer? quotient))
  (-> #:vars ([X Integer] [Y Integer])
      (rem X Y) (binary-op integer? remainder)))

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
     ; Division by zero throws an exception, so no rewrite
     #:= (RT (/ 1 0)) (T (/ 1 0))
     #:= (RT (< 1/3 1/2)) (T true)
     #:= (RT (> 1/3 1/2)) (T false)
     #:= (RT (<= 1/3 1/2)) (T true)
     #:= (RT (>= 1/3 1/2)) (T false)
     #:= (RT (<= 1/3 1/3)) (T true)
     #:= (RT (>= 1/3 1/3)) (T true))))

(define-context IEEE-floating-point
  (include IEEE-float-context)
  (-> #:vars ([X IEEE-binary32] [Y IEEE-binary32])
      (+ X Y) (binary-op single-flonum? +))
  (-> #:vars ([X IEEE-binary32] [Y IEEE-binary32])
      (- X Y) (binary-op single-flonum? -))
  (-> #:vars ([X IEEE-binary32] [Y IEEE-binary32])
      (* X Y) (binary-op single-flonum? *))
  (-> #:vars ([X IEEE-binary32] [Y IEEE-binary32])
      (/ X Y) (binary-op single-flonum? /))
  (-> #:vars ([X IEEE-binary32] [Y IEEE-binary32])
      (< X Y) (comparison-op single-flonum? <))
  (-> #:vars ([X IEEE-binary32] [Y IEEE-binary32])
      (> X Y) (comparison-op single-flonum? >))
  (-> #:vars ([X IEEE-binary32] [Y IEEE-binary32])
      (<= X Y) (comparison-op single-flonum? <=))
  (-> #:vars ([X IEEE-binary32] [Y IEEE-binary32])
      (>= X Y) (comparison-op single-flonum? >=))
  (-> #:vars ([X IEEE-binary64] [Y IEEE-binary64])
      (+ X Y) (binary-op double-flonum? +))
  (-> #:vars ([X IEEE-binary64] [Y IEEE-binary64])
      (- X Y) (binary-op double-flonum? -))
  (-> #:vars ([X IEEE-binary64] [Y IEEE-binary64])
      (* X Y) (binary-op double-flonum? *))
  (-> #:vars ([X IEEE-binary64] [Y IEEE-binary64])
      (/ X Y) (binary-op double-flonum? /))
  (-> #:vars ([X IEEE-binary64] [Y IEEE-binary64])
      (< X Y) (comparison-op double-flonum? <))
  (-> #:vars ([X IEEE-binary64] [Y IEEE-binary64])
      (> X Y) (comparison-op double-flonum? >))
  (-> #:vars ([X IEEE-binary64] [Y IEEE-binary64])
      (<= X Y) (comparison-op double-flonum? <=))
  (-> #:vars ([X IEEE-binary64] [Y IEEE-binary64])
      (>= X Y) (comparison-op double-flonum? >=)))

(module+ test
  (with-context IEEE-floating-point
    (chk
     #:= (RT (+ #x1s0 #x2s0)) (T #x3s0)
     #:= (RT (- #x1s0 #x2s0)) (T #x-1s0)
     #:= (RT (* #x1s0 #x2s0)) (T #x2s0)
     #:= (RT (/ #x1s0 #x2s0)) (T #x8s-1)
     #:= (RT (+ #x1l0 #x2l0)) (T #x3l0)
     #:= (RT (- #x1l0 #x2l0)) (T #x-1l0)
     #:= (RT (* #x1l0 #x2l0)) (T #x2l0)
     #:= (RT (/ #x1l0 #x2l0)) (T #x8l-1)
     #:= (RT (< #x1s0 #x2s0)) (T true)
     #:= (RT (> #x1s0 #x2s0)) (T false)
     #:= (RT (<= #x1s0 #x2s0)) (T true)
     #:= (RT (>= #x1s0 #x2s0)) (T false)
     #:= (RT (<= #x1s0 #x1s0)) (T true)
     #:= (RT (>= #x1s0 #x1s0)) (T true)
     #:= (RT (< #x1l0 #x2l0)) (T true)
     #:= (RT (> #x1l0 #x2l0)) (T false)
     #:= (RT (<= #x1l0 #x2l0)) (T true)
     #:= (RT (>= #x1l0 #x2l0)) (T false)
     #:= (RT (<= #x1l0 #x1l0)) (T true)
     #:= (RT (>= #x1l0 #x1l0)) (T true))))
