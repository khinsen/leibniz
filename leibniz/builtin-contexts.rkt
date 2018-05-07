#lang racket

(provide
 (contract-out
  [truth-signature signature?]
  [truth-rules rulelist?]
  [integer-signature signature?]
  [merged-integer-rules rulelist?]
  [rational-signature signature?]
  [merged-rational-rules rulelist?]
  [real-number-signature signature?]
  [merged-real-number-rules rulelist?]
  [IEEE-float-signature signature?]
  [merged-IEEE-float-rules rulelist?]))

(require "./builtins.rkt"
         "./operators.rkt"
         (prefix-in ss: "./signature-syntax.rkt")
         "./terms.rkt"
         "./equations.rkt"
         "./rewrite-syntax.rkt"
         threading)

(module+ test
  (require chk))

;
; Truth
;
(define truth-rules
  (~> empty-rulelist
      (add-rule
       (make-rule truth-signature
                  (make-term truth-signature
                             '_== (list (make-uvar truth-sorts 'X)
                                        (make-uvar truth-sorts 'Y)))
                  #f
                  (λ (signature pattern condition substitution)
                    (let ((x (substitution-value substitution 'X))
                          (y (substitution-value substitution 'Y)))
                      (make-term signature
                                 (if (equal? x y) 'true 'false)
                                 empty)))
                  #f #f))))

(module+ test
  (with-rules truth-signature truth-rules
    (chk
     #:= (RT (_== true false)) (T false)
     #:= (RT (_== true true)) (T true)
     #:= (RT (_== false true)) (T false)
     #:= (RT (_== false false)) (T true))))

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

(define (binary-op predicate/s proc)
  (define x-predicate (if (pair? predicate/s) (car predicate/s) predicate/s))
  (define y-predicate (if (pair? predicate/s) (cdr predicate/s) predicate/s))
  (λ (signature pattern condition substitution)
    (let ([x (substitution-value substitution 'X)]
          [y (substitution-value substitution 'Y)])
      (unless (and (x-predicate x) (y-predicate y))
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

(define integer-rules
  (rules integer-signature
         (-> #:vars ([X ℤ] [Y zero])
             (_+ X Y) (return-X))
         (-> #:vars ([X zero] [Y ℤ])
             (_+ X Y) (return-Y))
         (-> #:vars ([X ℤ] [Y ℤ])
             (_+ X Y) (binary-op integer? +))

         (-> #:vars ([X ℤ] [Y zero])
             (_- X Y) (return-X))
         (-> #:vars ([X ℤ] [Y ℤ])
             (_- X Y) (binary-op integer? -))
         (-> #:vars ([X ℤ])
             (- X) (unary-op integer? -))

         (=> #:vars ([X zero] [Y ℤ])
             (_× X Y) 0)
         (=> #:vars ([X ℤ] [Y zero])
             (_× X Y) 0)
         (-> #:vars ([X ℤ])
             (_× 1 X) (return-X))
         (-> #:vars ([X ℤ])
             (_× X 1) (return-X))
         (-> #:vars ([X ℤ] [Y ℤ])
             (_× X Y) (binary-op integer? *))

         (=> #:vars ([X zero] [Y ℤnz])
             (_div X Y) 0)
         (-> #:vars ([X ℤ])
             (_div X 1) (return-X))
         (-> #:vars ([X ℤ] [Y ℤnz])
             (_div X Y) (binary-op integer? quotient))

         (=> #:vars ([X zero] [Y ℤnz])
             (_rem X Y) 0)
         (=> #:vars ([X ℤ])
             (_rem X 1) 0)
         (-> #:vars ([X ℤ] [Y ℤ])
             (_rem X Y) (binary-op integer? remainder))

         (=> #:vars ([X ℤnz] [Y zero])
             (^ X Y) 1)
         (=> #:vars ([X ℕnz])
             (^ 0 X) 0)
         (-> #:vars ([X ℤ] [Y ℤ])
             (^ X Y) (binary-op integer? expt-with-fix-for-zero))

         (-> #:vars ([X ℤ])
             (abs X) (unary-op integer? abs))

         (-> #:vars ([X ℤ] [Y ℤ])
             (_= X Y) (comparison-op integer? =))
         (-> #:vars ([X ℤ] [Y ℤ])
             (_< X Y) (comparison-op integer? <))
         (-> #:vars ([X ℤ] [Y ℤ])
             (_> X Y) (comparison-op integer? >))
         (-> #:vars ([X ℤ] [Y ℤ])
             (_≤ X Y) (comparison-op integer? <=))
         (-> #:vars ([X ℤ] [Y ℤ])
             (_≥ X Y) (comparison-op integer? >=))))

(define merged-integer-rules
  (merge-rulelists truth-rules integer-rules integer-signature))

(module+ test
  (with-rules integer-signature merged-integer-rules
    (chk
     #:= (RT (_+ 2 3)) (T 5)
     #:= (RT (_- 2 3)) (T -1)
     #:= (RT (- 3)) (T -3)
     #:= (RT (_× 2 3)) (T 6)
     #:= (RT (_div 2 3)) (T 0)
     #:= (RT (_rem 2 3)) (T 2)
     #:= (RT (^ 2 3)) (T 8)
     #:= (RT (^ 2 0)) (T 1)
     #:= (RT (^ 0 0)) (T (^ 0 0)) ; Exception -> no rewrite
     #:= (RT (abs 3)) (T 3)
     #:= (RT (abs -3)) (T 3)
     #:= (RT (_= 2 3)) (T false)
     #:= (RT (_= 3 3)) (T true)
     #:= (RT (_< 2 3)) (T true)
     #:= (RT (_> 2 3)) (T false)
     #:= (RT (_≤ 2 3)) (T true)
     #:= (RT (_≥ 2 3)) (T false)
     #:= (RT (_≤ 3 3)) (T true)
     #:= (RT (_≥ 3 3)) (T true)
     #:= (RT (_== 3 3)) (T true)
     #:= (RT (_== 3 1)) (T false)))
  (define integer-signature+
    (ss:signature
     (include integer-signature)
     (op an-int ℤ)
     (op a-nzint ℤnz)
     (op a-nznat ℕnz)))
  (with-rules integer-signature+ merged-integer-rules
    (chk
     #:= (RT (_+ 0 an-int)) (T an-int)
     #:= (RT (_+ an-int 0)) (T an-int)
     #:= (RT (_- an-int 0)) (T an-int)
     #:= (RT (_× an-int 0)) (T 0)
     #:= (RT (_× 0 an-int)) (T 0)
     #:= (RT (_× an-int 1)) (T an-int)
     #:= (RT (_× 1 an-int)) (T an-int)
     #:= (RT (_div 0 a-nzint)) (T 0)
     #:= (RT (_div 0 an-int)) (T (_div 0 an-int))
     #:= (RT (_div an-int 1)) (T an-int)
     #:= (RT (_rem 0 a-nzint)) (T 0)
     #:= (RT (_rem 0 an-int)) (T (_rem 0 an-int))
     #:= (RT (_rem an-int 1)) (T 0)
     #:= (RT (^ a-nzint 0)) (T 1)
     #:= (RT (^ 0 a-nznat)) (T 0)
     #:= (RT (^ an-int 0)) (T (^ an-int 0))
     #:= (RT (^ 0 an-int)) (T (^ 0 an-int)))))

(define rational-rules
  (rules rational-signature
         (-> #:vars ([X ℚ] [Y ℚ])
             (_+ X Y) (binary-op exact? +))

         (-> #:vars ([X ℚ] [Y ℚ])
             (_- X Y) (binary-op exact? -))
         (-> #:vars ([X ℚ])
             (- X) (unary-op exact? -))

         (-> #:vars ([X ℚ] [Y ℚ])
             (_× X Y) (binary-op exact? *))

         (-> #:vars ([X ℚ] [Y ℚ])
             (_÷ X Y) (binary-op exact? /))

         (-> #:vars ([X ℚ] [Y ℚ])
             (^ X Y) (binary-op exact? expt-with-fix-for-zero))

         (-> #:vars ([X ℚ])
             (abs X) (unary-op exact? abs))

         (-> #:vars ([X ℚ] [Y ℚ])
             (_= X Y) (comparison-op exact? =))
         (-> #:vars ([X ℚ] [Y ℚ])
             (_< X Y) (comparison-op exact? <))
         (-> #:vars ([X ℚ] [Y ℚ])
             (_> X Y) (comparison-op exact? >))
         (-> #:vars ([X ℚ] [Y ℚ])
             (_≤ X Y) (comparison-op exact? <=))
         (-> #:vars ([X ℚ] [Y ℚ])
             (_≥ X Y) (comparison-op exact? >=))
         (-> #:vars ([X ℤ] [Y ℤ])
             (_div X Y) (binary-op integer? quotient))
         (-> #:vars ([X ℤ] [Y ℤ])
             (_rem X Y) (binary-op integer? remainder))))

(define merged-rational-rules**
  (merge-rulelists truth-rules rational-rules rational-signature))

(define rational-special-rules
  (rules rational-signature
         (-> #:vars ([X ℚ] [Y zero])
             (_+ X Y) (return-X))
         (-> #:vars ([X zero] [Y ℚ])
             (_+ X Y) (return-Y))

         (-> #:vars ([X ℚ] [Y zero])
             (_- X Y) (return-X))
         (-> #:vars ([X zero])
             (- X) 0)

         (=> #:vars ([X zero] [Y ℚ])
             (_× X Y) 0)
         (=> #:vars ([X ℚ] [Y zero])
             (_× X Y) 0)
         (-> #:vars ([X ℚ])
             (_× 1 X) (return-X))
         (-> #:vars ([X ℚ])
             (_× X 1) (return-X))

         (=> #:vars ([X zero] [Y ℚnz])
             (_÷ X Y) 0)
         (-> #:vars ([X ℚ])
             (_÷ X 1) (return-X))

         (=> #:vars ([X ℚnz] [Y zero])
             (^ X Y) 1)
         (=> #:vars ([X ℕnz])
             (^ 0 X) 0)))

(define merged-rational-rules
  (merge-rulelists merged-rational-rules**
                   rational-special-rules
                   rational-signature))

(module+ test
  (with-rules rational-signature merged-rational-rules
    (chk
     #:= (RT (_+ 2 3)) (T 5)
     #:= (RT (_+ 1/2 -1/2)) (T 0)
     #:= (RT (_- 2 3)) (T -1)
     #:= (RT (_- 1/2 1/2)) (T 0)
     #:= (RT (- 2/3)) (T -2/3)
     #:= (RT (_× 2 3)) (T 6)
     #:= (RT (_× 1/2 1/2)) (T 1/4)
     #:= (RT (_÷ 2 3)) (T 2/3)
     #:= (RT (_÷ 1/2 1/2)) (T 1)
     #:= (RT (_÷ 1 0)) (T (_÷ 1 0)) ; Exception -> no rewrite
     #:= (RT (^ 1/2 2)) (T 1/4)
     #:= (RT (^ -1/2 -2)) (T 4)
     #:= (RT (^ -1/2 0)) (T 1)
     #:= (RT (abs 3/2)) (T 3/2)
     #:= (RT (abs -3/2)) (T 3/2)
     #:= (RT (_= 1/3 1/2)) (T false)
     #:= (RT (_= 1/2 1/2)) (T true)
     #:= (RT (_< 1/3 1/2)) (T true)
     #:= (RT (_> 1/3 1/2)) (T false)
     #:= (RT (_≤ 1/3 1/2)) (T true)
     #:= (RT (_≥ 1/3 1/2)) (T false)
     #:= (RT (_≤ 1/3 1/3)) (T true)
     #:= (RT (_≥ 1/3 1/3)) (T true)
     #:= (RT (_== 1/3 1/3)) (T true)
     #:= (RT (_== 1/3 2/3)) (T false)))
  (define rational-signature+
    (ss:signature
     (include rational-signature)
     (op a-rat ℚ)
     (op a-nzrat ℚnz)
     (op a-nznat ℕnz)))
  (with-rules rational-signature+ merged-rational-rules
    (chk
     #:= (RT (_+ 0 a-rat)) (T a-rat)
     #:= (RT (_+ a-rat 0)) (T a-rat)
     #:= (RT (_- a-rat 0)) (T a-rat)
     #:= (RT (_× a-rat 0)) (T 0)
     #:= (RT (_× 0 a-rat)) (T 0)
     #:= (RT (_× a-rat 1)) (T a-rat)
     #:= (RT (_× 1 a-rat)) (T a-rat)
     #:= (RT (_÷ 0 a-nzrat)) (T 0)
     #:= (RT (_÷ 0 a-rat)) (T (_÷ 0 a-rat))
     #:= (RT (_÷ a-rat 1)) (T a-rat)
     #:= (RT (^ 0 a-nznat)) (T 0)
     #:= (RT (^ 0 a-rat)) (T (^ 0 a-rat))
     #:= (RT (^ a-nzrat 0)) (T 1)
     #:= (RT (^ a-rat 0)) (T (^ a-rat 0)))))

;
; Real numbers
;
; 'ℝ    real
; 'ℝnz  non-zero real
; 'ℝp   positive real
; 'ℝnn  non-negative real
;
(define real-number-signature
  (ss:signature
   (include rational-signature)
   (sort ℝ)
   (subsort ℚ ℝ)
   (sort ℝnz)
   (subsort ℝnz ℝ)
   (subsort ℚnz ℝnz)
   (sort ℝp)
   (subsort ℝp ℝnz)
   (subsort ℚp ℝp)
   (sort ℝnn)
   (subsort ℝnn ℝ)
   (subsort ℝp ℝnn)
   (subsort ℚnn ℝnn)
   (op (_+ ℝ ℝ) ℝ)
   (op (_+ ℝp ℝp) ℝp)
   (op (_+ ℝnn ℝnn) ℝnn)
   (op (_- ℝ ℝ) ℝ)
   (op (- ℝ) ℝ)
   (op (_× ℝ ℝ) ℝ)
   (op (_× ℝp ℝp) ℝp)
   (op (_× ℝnn ℝnn) ℝnn)
   (op (_÷ ℝ ℝnz) ℝ)
   (op (_÷ ℝnz ℝnz) ℝnz)
   (op (_÷ ℝp ℝp) ℝp)
   (op (_÷ ℝnn ℝp) ℝnn)
   (op (^ ℝp ℝnz) ℝp)
   (op (^ ℝnz ℤnz) ℝnz)
   (op (^ ℝ ℕnz) ℝ)
   (op (abs ℝ) ℝnn)
   (op (abs ℝnz) ℝp)
   (op (√ ℝnn) ℝnn)
   (op (√ ℝp) ℝp)
   (op (_= ℝ ℝ) boolean)
   (op (_< ℝ ℝ) boolean)
   (op (_> ℝ ℝ) boolean)
   (op (_≤ ℝ ℝ) boolean)
   (op (_≥ ℝ ℝ) boolean)))

(define real-number-rules
  (rules real-number-signature
   (-> #:vars ([X ℝ] [Y zero])
       (_+ X Y) (return-X))
   (-> #:vars ([X zero] [Y ℝ])
       (_+ X Y) (return-Y))

   (-> #:vars ([X ℝ] [Y zero])
       (_- X Y) (return-X))
   (-> #:vars ([X zero])
       (- X) (return-X))

   (=> #:vars ([X zero] [Y ℝ])
       (_× X Y) 0)
   (=> #:vars ([X ℝ] [Y zero])
       (_× X Y) 0)
   (-> #:vars ([X ℝ])
       (_× 1 X) (return-X))
   (-> #:vars ([X ℝ])
       (_× X 1) (return-X))

   (=> #:vars ([X zero] [Y ℝnz])
       (_÷ X Y) 0)
   (-> #:vars ([X ℝ])
       (_÷ X 1) (return-X))

   (=> #:vars ([X ℝnz] [Y zero])
       (^ X Y) 1)
   (=> #:vars ([X ℝp])
       (^ 0 X) 0)))

(define merged-real-number-rules
  (merge-rulelists merged-rational-rules** real-number-rules
                   real-number-signature))

(module+ test
  (define real-number-signature+
    (ss:signature
     (include real-number-signature)
     (op a-real ℝ)
     (op a-nzreal ℝnz)
     (op a-preal ℝp)))
  (with-rules real-number-signature+ merged-real-number-rules
    (chk
     #:= (RT (_+ 0 a-real)) (T a-real)
     #:= (RT (_+ a-real 0)) (T a-real)
     #:= (RT (_- a-real 0)) (T a-real)
     #:= (RT (_× a-real 0)) (T 0)
     #:= (RT (_× 0 a-real)) (T 0)
     #:= (RT (_× a-real 1)) (T a-real)
     #:= (RT (_× 1 a-real)) (T a-real)
     #:= (RT (_÷ 0 a-nzreal)) (T 0)
     #:= (RT (_÷ 0 a-real)) (T (_÷ 0 a-real))
     #:= (RT (_÷ a-real 1)) (T a-real)
     #:= (RT (^ 0 a-preal)) (T 0)
     #:= (RT (^ 0 a-real)) (T (^ 0 a-real))
     #:= (RT (^ a-nzreal 0)) (T 1)
     #:= (RT (^ a-real 0)) (T (^ a-real 0)))))

;
; Floating-point numbers (IEEE binary32 and binary64)
;
(define IEEE-float-rules
  (rules IEEE-float-signature
         (-> #:vars ([X FP32] [Y FP32])
             (_+ X Y) (binary-op single-flonum? +))
         (-> #:vars ([X FP32] [Y FP32])
             (_- X Y) (binary-op single-flonum? -))
         (-> #:vars ([X FP32])
             (- X) (unary-op single-flonum? -))
         (-> #:vars ([X FP32] [Y FP32])
             (_× X Y) (binary-op single-flonum? *))
         (-> #:vars ([X FP32] [Y FP32])
             (_÷ X Y) (binary-op single-flonum? /))
         (-> #:vars ([X FP32] [Y FP32])
             (^ X Y) (binary-op single-flonum? expt-with-fix-for-zero))
         (-> #:vars ([X FP32] [Y ℤ])
             (^ X Y) (binary-op (cons single-flonum? integer?) expt-with-fix-for-zero))
         (-> #:vars ([X FP32])
             (abs X) (unary-op single-flonum? abs))
         (-> #:vars ([X FP32])
             (√ X) (unary-op single-flonum? sqrt))
         (-> #:vars ([X FP32] [Y FP32])
             (_= X Y) (comparison-op single-flonum? =))
         (-> #:vars ([X FP32] [Y FP32])
             (_< X Y) (comparison-op single-flonum? <))
         (-> #:vars ([X FP32] [Y FP32])
             (_> X Y) (comparison-op single-flonum? >))
         (-> #:vars ([X FP32] [Y FP32])
             (_≤ X Y) (comparison-op single-flonum? <=))
         (-> #:vars ([X FP32] [Y FP32])
             (_≥ X Y) (comparison-op single-flonum? >=))
         (-> #:vars ([X FP64] [Y FP64])
             (_+ X Y) (binary-op double-flonum? +))
         (-> #:vars ([X FP64] [Y FP64])
             (_- X Y) (binary-op double-flonum? -))
         (-> #:vars ([X FP64])
             (- X) (unary-op double-flonum? -))
         (-> #:vars ([X FP64] [Y FP64])
             (_× X Y) (binary-op double-flonum? *))
         (-> #:vars ([X FP64] [Y FP64])
             (_÷ X Y) (binary-op double-flonum? /))
         (-> #:vars ([X FP64] [Y FP64])
             (^ X Y) (binary-op double-flonum? expt-with-fix-for-zero))
         (-> #:vars ([X FP64] [Y ℤ])
             (^ X Y) (binary-op (cons double-flonum? integer?) expt-with-fix-for-zero))
         (-> #:vars ([X FP64])
             (abs X) (unary-op double-flonum? abs))
         (-> #:vars ([X FP64])
             (√ X) (unary-op double-flonum? sqrt))
         (-> #:vars ([X FP64] [Y FP64])
             (_= X Y) (comparison-op double-flonum? =))
         (-> #:vars ([X FP64] [Y FP64])
             (_< X Y) (comparison-op double-flonum? <))
         (-> #:vars ([X FP64] [Y FP64])
             (_> X Y) (comparison-op double-flonum? >))
         (-> #:vars ([X FP64] [Y FP64])
             (_≤ X Y) (comparison-op double-flonum? <=))
         (-> #:vars ([X FP64] [Y FP64])
             (_≥ X Y) (comparison-op double-flonum? >=))))

(define merged-IEEE-float-rules
  (merge-rulelists merged-integer-rules IEEE-float-rules IEEE-float-signature))

(module+ test
  (with-rules IEEE-float-signature merged-IEEE-float-rules
    (chk
     #:= (RT (_+ #x1s0 #x2s0)) (T #x3s0)
     #:= (RT (_- #x1s0 #x2s0)) (T #x-1s0)
     #:= (RT (- #x1s0)) (T #x-1s0)
     #:= (RT (_× #x1s0 #x2s0)) (T #x2s0)
     #:= (RT (_÷ #x1s0 #x2s0)) (T #x8s-1)
     #:= (RT (^ #x2s0 #x1s0)) (T #x2s0)
     #:= (RT (^ #x2s0 2)) (T #x4s0)
     #:= (RT (abs #x1s0)) (T #x1s0)
     #:= (RT (√ #x4s0)) (T #x2s0)
     #:= (RT (_+ #x1l0 #x2l0)) (T #x3l0)
     #:= (RT (_- #x1l0 #x2l0)) (T #x-1l0)
     #:= (RT (- #x1l0)) (T #x-1l0)
     #:= (RT (_× #x1l0 #x2l0)) (T #x2l0)
     #:= (RT (_÷ #x1l0 #x2l0)) (T #x8l-1)
     #:= (RT (^ #x2l0 #x1l0)) (T #x2l0)
     #:= (RT (^ #x2l0 2)) (T #x4l0)
     #:= (RT (abs #x1l0)) (T #x1l0)
     #:= (RT (√ #x4l0)) (T #x2l0)
     #:= (RT (_= #x1s0 #x2s0)) (T false)
     #:= (RT (_= #x1s0 #x1s0)) (T true)
     #:= (RT (_< #x1s0 #x2s0)) (T true)
     #:= (RT (_> #x1s0 #x2s0)) (T false)
     #:= (RT (_≤ #x1s0 #x2s0)) (T true)
     #:= (RT (_≥ #x1s0 #x2s0)) (T false)
     #:= (RT (_≤ #x1s0 #x1s0)) (T true)
     #:= (RT (_≥ #x1s0 #x1s0)) (T true)
     #:= (RT (_= #x1l0 #x2l0)) (T false)
     #:= (RT (_= #x1l0 #x1l0)) (T true)
     #:= (RT (_< #x1l0 #x2l0)) (T true)
     #:= (RT (_> #x1l0 #x2l0)) (T false)
     #:= (RT (_≤ #x1l0 #x2l0)) (T true)
     #:= (RT (_≥ #x1l0 #x2l0)) (T false)
     #:= (RT (_≤ #x1l0 #x1l0)) (T true)
     #:= (RT (_≥ #x1l0 #x1l0)) (T true)
     #:= (RT (_== #x1l0 #x1l0)) (T true)
     #:= (RT (_== #x1l0 #x2l0)) (T false))))

