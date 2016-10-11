#lang racket

(provide
 (contract-out
  [truth context?]
  [boolean context?]
  [integers context?]
  [rational-numbers context?]
  [real-numbers context?]
  [IEEE-floating-point context?]))

(require "./builtins.rkt"
         "./terms.rkt"
         "./equations.rkt"
         (only-in "./contexts.rkt" context? define-context builtin-context)
         threading)

(module+ test
  (require chk
           "./context-syntax.rkt"))

;
; Truth
;
(define truth
  (builtin-context
   truth-sorts truth-signature
   (empty-varset truth-sorts)
   (~> empty-rulelist
       (add-rule
        (make-rule truth-signature
                   (make-term truth-signature
                              '== (list (make-uvar truth-sorts 'X)
                                        (make-uvar truth-sorts 'Y)))
                   #f
                   (λ (signature pattern condition substitution)
                     (let ((x (substitution-value substitution 'X))
                           (y (substitution-value substitution 'Y)))
                       (make-term signature
                                  (if (equal? x y) 'true 'false)
                                  empty)))
                   #f #f)))
   empty-equationset))

(module+ test
  (with-context truth
    (chk
     #:= (RT (== true false)) (T false)
     #:= (RT (== true true)) (T true)
     #:= (RT (== false true)) (T false)
     #:= (RT (== false false)) (T true))))

;
; Boolean algebra
;
; Adapted from
; http://maude.cs.uiuc.edu/maude1/manual/maude-manual-html/maude-manual_16.html
;
(define-context boolean
  (include truth)
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
     #:= (RT (=> false false))  (T true)
     #:= (RT (== true true))    (T true)
     #:= (RT (== true false))   (T false)
     #:= (RT (== false true))   (T false)
     #:= (RT (== false false))  (T true))))

;
; Symbols and strings (to be completed)
;
(define symbol*
  (builtin-context symbol-sorts symbol-signature
                   (empty-varset symbol-sorts) empty-rulelist empty-equationset))

(define string*
  (builtin-context string-sorts string-signature
                   (empty-varset string-sorts) empty-rulelist empty-equationset))

;
; Integers and rational numbers
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

(define integer*
  (builtin-context integer-sorts integer-signature
                   (empty-varset integer-sorts)
                   empty-rulelist empty-equationset))

(define-context integers
  (include integer*)
  (include truth)
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
     #:= (RT (>= 3 3)) (T true)
     #:= (RT (== 3 3)) (T true)
     #:= (RT (== 3 1)) (T false))))

(define rationals*
  (builtin-context rational-sorts rational-signature
                   (empty-varset rational-sorts)
                   empty-rulelist empty-equationset))

(define-context rational-numbers
  (include rationals*)
  (include truth)
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
  (with-context rational-numbers
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
     #:= (RT (>= 1/3 1/3)) (T true)
     #:= (RT (== 1/3 1/3)) (T true)
     #:= (RT (== 1/3 2/3)) (T false))))

;
; Real numbers
;
(define-context real-numbers
  (include rational-numbers)
  (sort Real)
  (subsort Rational Real)
  (sort NonZeroReal)
  (subsort NonZeroReal Real)
  (subsort NonZeroRational NonZeroReal)
  (sort PositiveReal)
  (subsort PositiveReal NonZeroReal)
  (subsort PositiveRational PositiveReal)
  (sort NonNegativeReal)
  (subsort PositiveReal NonNegativeReal)
  (subsort NonNegativeRational NonNegativeReal)
  (op (+ Real Real) Real)
  (op (+ PositiveReal PositiveReal) PositiveReal)
  (op (+ NonNegativeReal NonNegativeReal) NonNegativeReal)
  (op (- Real Real) Real)
  (op (* Real Real) Real)
  (op (* PositiveReal PositiveReal) PositiveReal)
  (op (* NonNegativeReal NonNegativeReal) NonNegativeReal)
  (op (* Zero Real) Zero)
  (op (* Real Zero) Zero)
  (op (/ Real NonZeroReal) Real)
  (op (/ PositiveReal NonZeroReal) PositiveReal)
  (op (/ NonNegativeReal NonZeroReal) NonNegativeReal)
  (op (/ Zero NonZeroReal) Zero)
  (op (< Real Real) Boolean)
  (op (> Real Real) Boolean)
  (op (<= Real Real) Boolean)
  (op (>= Real Real) Boolean))

;
; Floating-point numbers (IEEE binary32 and binary64)
;
(define IEEE-float*
  (builtin-context IEEE-float-sorts IEEE-float-signature
                   (empty-varset IEEE-float-sorts)
                   empty-rulelist empty-equationset))

(define-context IEEE-floating-point
  (include IEEE-float*)
  (include truth)
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
     #:= (RT (>= #x1l0 #x1l0)) (T true)
     #:= (RT (== #x1l0 #x1l0)) (T true)
     #:= (RT (== #x1l0 #x2l0)) (T false))))
