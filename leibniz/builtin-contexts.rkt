#lang racket

(provide
 (contract-out
  [truth context?]
  [boolean context?]
  [integers context?]
  [rational-numbers context?]
  [real-numbers context?]
  [IEEE-floating-point context?]
  [IEEE-floating-point-with-conversion context?]))

(require "./builtins.rkt"
         "./terms.rkt"
         "./equations.rkt"
         (only-in "./contexts.rkt" context? define-context builtin-context)
         threading)

(module+ test
  (require chk
           "./rewrite-syntax.rkt"))

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
(define (return-X)
  (λ (signature pattern condition substitution)
    (substitution-value substitution 'X)))

(define (return-Y)
  (λ (signature pattern condition substitution)
    (substitution-value substitution 'Y)))

(define (unary-op predicate proc)
  (λ (signature pattern condition substitution)
    (let ([x (substitution-value substitution 'X)])
      (unless (predicate x) 
        (error "argument not a number"))
      (proc x))))

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

(define (expt-with-fix-for-zero x y)
  (when (and (eq? x 0)
             (eq? y 0))
    (error "expt: undefined for 0 and 0"))
  (expt x y))

(define integer*
  (builtin-context integer-sorts integer-signature
                   (empty-varset integer-sorts)
                   empty-rulelist empty-equationset))

(define-context integers
  (include integer*)
  (include truth)

  (-> #:vars ([X Integer] [Y Zero])
      (+ X Y) (return-X))
  (-> #:vars ([X Zero] [Y Integer])
      (+ X Y) (return-Y))
  (-> #:vars ([X Integer] [Y Integer])
      (+ X Y) (binary-op integer? +))

  (-> #:vars ([X Integer] [Y Zero])
      (- X Y) (return-X))
  (-> #:vars ([X Integer] [Y Integer])
      (- X Y) (binary-op integer? -))

  (=> #:vars ([X Zero] [Y Integer])
      (* X Y) 0)
  (=> #:vars ([X Integer] [Y Zero])
      (* X Y) 0)
  (-> #:vars ([X Integer])
      (* 1 X) (return-X))
  (-> #:vars ([X Integer])
      (* X 1) (return-X))
  (-> #:vars ([X Integer] [Y Integer])
      (* X Y) (binary-op integer? *))

  (=> #:vars ([X Zero] [Y NonZeroInteger])
      (div X Y) 0)
  (-> #:vars ([X Integer])
      (div X 1) (return-X))
  (-> #:vars ([X Integer] [Y NonZeroInteger])
      (div X Y) (binary-op integer? quotient))

  (=> #:vars ([X Zero] [Y NonZeroInteger])
      (rem X Y) 0)
  (=> #:vars ([X Integer])
      (rem X 1) 0)
  (-> #:vars ([X Integer] [Y Integer])
      (rem X Y) (binary-op integer? remainder))

  (=> #:vars ([X NonZeroInteger] [Y Zero])
      (^ X Y) 1)
  (=> #:vars ([X NonZeroNatural])
      (^ 0 X) 0)
  (-> #:vars ([X Integer] [Y Integer])
      (^ X Y) (binary-op integer? expt-with-fix-for-zero))

  (-> #:vars ([X Integer])
      (abs X) (unary-op integer? abs))

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
     #:= (RT (^ 2 3)) (T 8)
     #:= (RT (^ 2 0)) (T 1)
     #:= (RT (^ 0 0)) (T (^ 0 0)) ; Exception -> no rewrite
     #:= (RT (abs 3)) (T 3)
     #:= (RT (abs -3)) (T 3)
     #:= (RT (< 2 3)) (T true)
     #:= (RT (> 2 3)) (T false)
     #:= (RT (<= 2 3)) (T true)
     #:= (RT (>= 2 3)) (T false)
     #:= (RT (<= 3 3)) (T true)
     #:= (RT (>= 3 3)) (T true)
     #:= (RT (== 3 3)) (T true)
     #:= (RT (== 3 1)) (T false)))
  (define-context integers+
    (include integers)
    (op an-int Integer)
    (op a-nzint NonZeroInteger)
    (op a-nznat NonZeroNatural))
  (with-context integers+
    (chk
     #:= (RT (+ 0 an-int)) (T an-int)
     #:= (RT (+ an-int 0)) (T an-int)
     #:= (RT (- an-int 0)) (T an-int)
     #:= (RT (* an-int 0)) (T 0)
     #:= (RT (* 0 an-int)) (T 0)
     #:= (RT (* an-int 1)) (T an-int)
     #:= (RT (* 1 an-int)) (T an-int)
     #:= (RT (div 0 a-nzint)) (T 0)
     #:= (RT (div 0 an-int)) (T (div 0 an-int))
     #:= (RT (div an-int 1)) (T an-int)
     #:= (RT (rem 0 a-nzint)) (T 0)
     #:= (RT (rem 0 an-int)) (T (rem 0 an-int))
     #:= (RT (rem an-int 1)) (T 0)
     #:= (RT (^ a-nzint 0)) (T 1)
     #:= (RT (^ 0 a-nznat)) (T 0)
     #:= (RT (^ an-int 0)) (T (^ an-int 0))
     #:= (RT (^ 0 an-int)) (T (^ 0 an-int)))))

(define rationals*
  (builtin-context rational-sorts rational-signature
                   (empty-varset rational-sorts)
                   empty-rulelist empty-equationset))

(define-context rationals**
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
      (^ X Y) (binary-op exact? expt-with-fix-for-zero))

  (-> #:vars ([X Rational])
      (abs X) (unary-op exact? abs))

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

(define-context rational-numbers
  (include rationals**)

  (-> #:vars ([X Rational] [Y Zero])
      (+ X Y) (return-X))
  (-> #:vars ([X Zero] [Y Rational])
      (+ X Y) (return-Y))

  (-> #:vars ([X Rational] [Y Zero])
      (- X Y) (return-X))

  (=> #:vars ([X Zero] [Y Rational])
      (* X Y) 0)
  (=> #:vars ([X Rational] [Y Zero])
      (* X Y) 0)
  (-> #:vars ([X Rational])
      (* 1 X) (return-X))
  (-> #:vars ([X Rational])
      (* X 1) (return-X))

  (=> #:vars ([X Zero] [Y NonZeroRational])
      (/ X Y) 0)
  (-> #:vars ([X Rational])
      (/ X 1) (return-X))

  (=> #:vars ([X NonZeroRational] [Y Zero])
      (^ X Y) 1)
  (=> #:vars ([X NonZeroNatural])
      (^ 0 X) 0))

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
     #:= (RT (/ 1 0)) (T (/ 1 0)) ; Exception -> no rewrite
     #:= (RT (^ 1/2 2)) (T 1/4)
     #:= (RT (^ -1/2 -2)) (T 4)
     #:= (RT (^ -1/2 0)) (T 1)
     #:= (RT (abs 3/2)) (T 3/2)
     #:= (RT (abs -3/2)) (T 3/2)
     #:= (RT (< 1/3 1/2)) (T true)
     #:= (RT (> 1/3 1/2)) (T false)
     #:= (RT (<= 1/3 1/2)) (T true)
     #:= (RT (>= 1/3 1/2)) (T false)
     #:= (RT (<= 1/3 1/3)) (T true)
     #:= (RT (>= 1/3 1/3)) (T true)
     #:= (RT (== 1/3 1/3)) (T true)
     #:= (RT (== 1/3 2/3)) (T false)))
  (define-context rational-numbers+
    (include rational-numbers)
    (op a-rat Rational)
    (op a-nzrat NonZeroRational)
    (op a-nznat NonZeroNatural))
  (with-context rational-numbers+
    (chk
     #:= (RT (+ 0 a-rat)) (T a-rat)
     #:= (RT (+ a-rat 0)) (T a-rat)
     #:= (RT (- a-rat 0)) (T a-rat)
     #:= (RT (* a-rat 0)) (T 0)
     #:= (RT (* 0 a-rat)) (T 0)
     #:= (RT (* a-rat 1)) (T a-rat)
     #:= (RT (* 1 a-rat)) (T a-rat)
     #:= (RT (/ 0 a-nzrat)) (T 0)
     #:= (RT (/ 0 a-rat)) (T (/ 0 a-rat))
     #:= (RT (/ a-rat 1)) (T a-rat)
     #:= (RT (^ 0 a-nznat)) (T 0)
     #:= (RT (^ 0 a-rat)) (T (^ 0 a-rat))
     #:= (RT (^ a-nzrat 0)) (T 1)
     #:= (RT (^ a-rat 0)) (T (^ a-rat 0)))))

;
; Real numbers
;
(define-context real-numbers
  (include rationals**)
  (sort Real)
  (subsort Rational Real)
  (sort NonZeroReal)
  (subsort NonZeroReal Real)
  (subsort NonZeroRational NonZeroReal)
  (sort PositiveReal)
  (subsort PositiveReal NonZeroReal)
  (subsort PositiveRational PositiveReal)
  (sort NonNegativeReal)
  (subsort NonNegativeReal Real)
  (subsort PositiveReal NonNegativeReal)
  (subsort NonNegativeRational NonNegativeReal)
  (op (+ Real Real) Real)
  (op (+ PositiveReal PositiveReal) PositiveReal)
  (op (+ NonNegativeReal NonNegativeReal) NonNegativeReal)
  (op (- Real Real) Real)
  (op (* Real Real) Real)
  (op (* PositiveReal PositiveReal) PositiveReal)
  (op (* NonNegativeReal NonNegativeReal) NonNegativeReal)
  (op (/ Real NonZeroReal) Real)
  (op (/ NonZeroReal NonZeroReal) NonZeroReal)
  (op (/ PositiveReal PositiveReal) PositiveReal)
  (op (/ NonNegativeReal PositiveReal) NonNegativeReal)
  (op (^ PositiveReal NonZeroReal) PositiveReal)
  (op (^ NonZeroReal NonZeroInteger) NonZeroReal)
  (op (abs Real) NonNegativeReal)
  (op (abs NonZeroReal) PositiveReal)
  (op (√ NonNegativeReal) NonNegativeReal)
  (op (√ PositiveReal) PositiveReal)
  (op (< Real Real) Boolean)
  (op (> Real Real) Boolean)
  (op (<= Real Real) Boolean)
  (op (>= Real Real) Boolean)

  (-> #:vars ([X Real] [Y Zero])
      (+ X Y) (return-X))
  (-> #:vars ([X Zero] [Y Real])
      (+ X Y) (return-Y))

  (-> #:vars ([X Real] [Y Zero])
      (- X Y) (return-X))

  (=> #:vars ([X Zero] [Y Real])
      (* X Y) 0)
  (=> #:vars ([X Real] [Y Zero])
      (* X Y) 0)
  (-> #:vars ([X Real])
      (* 1 X) (return-X))
  (-> #:vars ([X Real])
      (* X 1) (return-X))

  (=> #:vars ([X Zero] [Y NonZeroReal])
      (/ X Y) 0)
  (-> #:vars ([X Real])
      (/ X 1) (return-X))

  (=> #:vars ([X NonZeroReal] [Y Zero])
      (^ X Y) 1)
  (=> #:vars ([X PositiveReal])
      (^ 0 X) 0))

(module+ test
  (define-context real-numbers+
    (include real-numbers)
    (op a-real Real)
    (op a-nzreal NonZeroReal)
    (op a-preal PositiveReal))
  (with-context real-numbers+
    (chk
     #:= (RT (+ 0 a-real)) (T a-real)
     #:= (RT (+ a-real 0)) (T a-real)
     #:= (RT (- a-real 0)) (T a-real)
     #:= (RT (* a-real 0)) (T 0)
     #:= (RT (* 0 a-real)) (T 0)
     #:= (RT (* a-real 1)) (T a-real)
     #:= (RT (* 1 a-real)) (T a-real)
     #:= (RT (/ 0 a-nzreal)) (T 0)
     #:= (RT (/ 0 a-real)) (T (/ 0 a-real))
     #:= (RT (/ a-real 1)) (T a-real)
     #:= (RT (^ 0 a-preal)) (T 0)
     #:= (RT (^ 0 a-real)) (T (^ 0 a-real))
     #:= (RT (^ a-nzreal 0)) (T 1)
     #:= (RT (^ a-real 0)) (T (^ a-real 0)))))

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
      (^ X Y) (binary-op single-flonum? expt-with-fix-for-zero))
  (-> #:vars ([X IEEE-binary32])
      (abs X) (unary-op single-flonum? abs))
  (-> #:vars ([X IEEE-binary32])
      (√ X) (unary-op single-flonum? sqrt))
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
      (^ X Y) (binary-op double-flonum? expt-with-fix-for-zero))
  (-> #:vars ([X IEEE-binary64])
      (abs X) (unary-op double-flonum? abs))
  (-> #:vars ([X IEEE-binary64])
      (√ X) (unary-op double-flonum? sqrt))
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
     #:= (RT (abs #x1s0)) (T #x1s0)
     #:= (RT (√ #x4s0)) (T #x2s0)
     #:= (RT (+ #x1l0 #x2l0)) (T #x3l0)
     #:= (RT (- #x1l0 #x2l0)) (T #x-1l0)
     #:= (RT (* #x1l0 #x2l0)) (T #x2l0)
     #:= (RT (/ #x1l0 #x2l0)) (T #x8l-1)
     #:= (RT (abs #x1l0)) (T #x1l0)
     #:= (RT (√ #x4l0)) (T #x2l0)
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

(define-context IEEE-floating-point-with-conversion
  (include IEEE-floating-point)
  (include integers)
  (op (Integer->IEEE-binary32 Integer) IEEE-binary32)
  (op (Integer->IEEE-binary64 Integer) IEEE-binary64)
  (-> #:vars ([X Integer])
      (Integer->IEEE-binary32 X) (unary-op integer? real->single-flonum))
  (-> #:vars ([X Integer])
      (Integer->IEEE-binary64 X) (unary-op integer? real->double-flonum)))

(module+ test
  (with-context IEEE-floating-point-with-conversion
    (chk
     #:= (RT (Integer->IEEE-binary32 1)) (T #x1s0)
     #:= (RT (Integer->IEEE-binary64 1)) (T #x1l0))))
