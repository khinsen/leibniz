#lang racket

(provide number-term.sort number-term.builtin-type
         truth-sorts truth-signature
         symbol-sorts symbol-signature
         string-sorts string-signature
         integer-sorts integer-signature
         rational-sorts rational-signature
         IEEE-float-sorts IEEE-float-signature)

(require "./sorts.rkt"
         "./operators.rkt"
         threading)

(module+ test
  (require rackunit racket/function)
  (define (sort-of-noarg-term signature name)
    (define rank (lookup-op signature name empty))
    (and rank
         (cdr rank)))
  (define (sort-of-numarg-term signature name args)
    (define rank (lookup-op signature name (map number-term.sort args)))
    (and rank
         (cdr rank))))

;
; Booleans
;
; These are standard operator-argument terms, but they are built
; in because built-in operations such as equality testing depend
; on them.
;
(define truth-sorts
  (~> empty-sort-graph
      (add-sort 'boolean)))

(define truth-signature
  (~> (empty-signature truth-sorts #:builtins (set '*truth*))
      (add-op 'true empty 'boolean)
      (add-op 'false empty 'boolean)))

(module+ test
  (check-equal? (sort-of-noarg-term truth-signature 'true) 'boolean)
  (check-equal? (sort-of-noarg-term truth-signature 'false) 'boolean)
  (check-false (non-regular-op-example truth-signature)))

;
; The remaining built-in term types are special terms in that they
; do not follow the operator-arguments structure. These are the standard
; fundamental data types that most programming languages offer.
;

;
; Symbols
;
(define symbol-sorts
  (~> empty-sort-graph
      (add-sort 'symbol)))

(define symbol-signature
  (empty-signature symbol-sorts #:builtins (set '*symbol*)))

;
; Strings
;
(define string-sorts
  (~> empty-sort-graph
      (add-sort 'string)))

(define string-signature
  (~> (empty-signature string-sorts #:builtins (set '*string*))
      (add-op '_+ (list 'string 'string) 'string)))

;
; Integers and their subsets
;
; ℕ    natural
; ℕ.nz non-zero natural
; ℤ    integer
; ℤ.nz non-zero integer
;
; "truth" is included because comparisons have sort boolean.
;
(define integer-sorts
  (~> truth-sorts
      ; Natural numbers
      (add-sort 'ℕ)
      (add-sort 'zero)
      (add-subsort-relation 'zero 'ℕ)
      (add-sort 'ℕ.nz)
      (add-subsort-relation 'ℕ.nz 'ℕ)
      ; Integers
      (add-sort 'ℤ)
      (add-subsort-relation 'ℕ 'ℤ)
      (add-sort 'ℤ.nz)
      (add-subsort-relation 'ℤ.nz 'ℤ)
      (add-subsort-relation 'ℕ.nz 'ℤ.nz)))

(define integer-signature
  (~> (empty-signature integer-sorts #:builtins (set '*integer*))
      (merge-signatures truth-signature #f)
      (add-op '_+ (list 'ℤ 'ℤ) 'ℤ)
      (add-op '_+ (list 'ℕ 'ℕ) 'ℕ)
      (add-op '_+ (list 'ℕ.nz 'ℕ) 'ℕ.nz)
      (add-op '_+ (list 'ℕ 'ℕ.nz) 'ℕ.nz)
      (add-op '_- (list 'ℤ 'ℤ) 'ℤ)
      (add-op '- (list 'ℤ) 'ℤ)
      (add-op '_× (list 'ℤ 'ℤ) 'ℤ)
      (add-op '_× (list 'ℕ 'ℕ) 'ℕ)
      (add-op '_× (list 'ℕ.nz 'ℕ.nz) 'ℕ.nz)
      (add-op '_× (list 'ℤ.nz 'ℤ.nz) 'ℤ.nz)
      (add-op '_div (list 'ℤ 'ℤ.nz) 'ℤ)
      (add-op '_div (list 'ℕ 'ℕ.nz) 'ℕ)
      (add-op '_rem (list 'ℤ 'ℤ.nz) 'ℤ)
      (add-op '_rem (list 'ℕ 'ℕ.nz) 'ℕ)
      (add-op '^ (list 'ℤ 'ℕ.nz) 'ℤ)
      (add-op '^ (list 'ℕ 'ℕ.nz) 'ℕ)
      (add-op '^ (list 'ℕ.nz 'ℕ.nz) 'ℕ.nz)
      (add-op '^ (list 'ℤ.nz 'ℕ.nz) 'ℤ.nz)
      (add-op '^ (list 'ℤ.nz 'zero) 'ℕ.nz)
      (add-op 'abs (list 'ℤ) 'ℕ)
      (add-op 'abs (list 'ℤ.nz) 'ℕ.nz)
      (add-op '_= (list 'ℤ 'ℤ) 'boolean)
      (add-op '_< (list 'ℤ 'ℤ) 'boolean)
      (add-op '_> (list 'ℤ 'ℤ) 'boolean)
      (add-op '_≤ (list 'ℤ 'ℤ) 'boolean)
      (add-op '_≥ (list 'ℤ 'ℤ) 'boolean)))

(module+ test
  (check-equal? (sort-of-numarg-term integer-signature '_+ (list 1 2)) 'ℕ.nz)
  (check-equal? (sort-of-numarg-term integer-signature '_+ (list 0 2)) 'ℕ.nz)
  (check-equal? (sort-of-numarg-term integer-signature '_+ (list 0 0)) 'ℕ)
  (check-equal? (sort-of-numarg-term integer-signature '_+ (list -1 0)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_- (list 0 0)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '- (list 1)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_× (list 1 2)) 'ℕ.nz)
  (check-equal? (sort-of-numarg-term integer-signature '_× (list 0 2)) 'ℕ)
  (check-equal? (sort-of-numarg-term integer-signature '_× (list -2 0)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_× (list -2 -2)) 'ℤ.nz)
  (check-equal? (sort-of-numarg-term integer-signature '_div (list -2 -2)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_div (list 0 1)) 'ℕ)
  (check-equal? (sort-of-numarg-term integer-signature '_rem (list -2 -2)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_rem (list 0 1)) 'ℕ)
  (check-equal? (sort-of-numarg-term integer-signature '^ (list 2 1)) 'ℕ.nz)
  (check-equal? (sort-of-numarg-term integer-signature '^ (list 2 0))
                (number-term.sort 1))
  (check-equal? (sort-of-numarg-term integer-signature '^ (list -2 1)) 'ℤ.nz)
  (check-equal? (sort-of-numarg-term integer-signature '^ (list -2 0))
                (number-term.sort 1))
  (check-equal? (sort-of-numarg-term integer-signature '^ (list 0 0))
                (kind integer-sorts 'ℤ))
  (check-equal? (sort-of-numarg-term integer-signature '_= (list 0 1)) 'boolean)
  (check-equal? (sort-of-numarg-term integer-signature '_< (list 0 1)) 'boolean)
  (check-equal? (sort-of-numarg-term integer-signature '_> (list 0 1)) 'boolean)
  (check-equal? (sort-of-numarg-term integer-signature '_≤ (list 0 1)) 'boolean)
  (check-equal? (sort-of-numarg-term integer-signature '_≥ (list 0 1)) 'boolean)
  (check-equal? (sort-of-numarg-term integer-signature '_== (list 0 1)) 'boolean)
  (check-false (non-regular-op-example integer-signature)))

;
; Rationals and their subsets
; 
; ℚ     rational
; ℚ.nz  non-zero rational
; ℚ.p   positive rational
; ℚ.nn  non-negative rational
;
(define rational-sorts
  (~> integer-sorts
      ; Rational numbers
      (add-sort 'ℚ)
      (add-subsort-relation 'ℤ 'ℚ)
      (add-sort 'ℚ.nz)
      (add-subsort-relation 'ℚ.nz 'ℚ)
      (add-subsort-relation 'ℤ.nz 'ℚ.nz)
      (add-sort 'ℚ.p)
      (add-subsort-relation 'ℚ.p 'ℚ.nz)
      (add-subsort-relation 'ℕ.nz 'ℚ.p)
      (add-sort 'ℚ.nn)
      (add-subsort-relation 'ℚ.nn 'ℚ)
      (add-subsort-relation 'ℚ.p 'ℚ.nn)
      (add-subsort-relation 'ℕ 'ℚ.nn)))

(define rational-signature
  (~> (empty-signature rational-sorts #:builtins (set '*rational*))
      (merge-signatures integer-signature #f)
      (add-op '_+ (list 'ℚ 'ℚ) 'ℚ)
      (add-op '_+ (list 'ℚ.p 'ℚ.p) 'ℚ.p)
      (add-op '_+ (list 'ℚ.nn 'ℚ.nn) 'ℚ.nn)
      (add-op '_- (list 'ℚ 'ℚ) 'ℚ)
      (add-op '- (list 'ℚ) 'ℚ)
      (add-op '_× (list 'ℚ 'ℚ) 'ℚ)
      (add-op '_× (list 'ℚ.p 'ℚ.p) 'ℚ.p)
      (add-op '_× (list 'ℚ.nn 'ℚ.nn) 'ℚ.nn)
      (add-op '_× (list 'ℚ.nz 'ℚ.nz) 'ℚ.nz)
      (add-op '_÷ (list 'ℚ 'ℚ.nz) 'ℚ)
      (add-op '_÷ (list 'ℚ.nz 'ℚ.nz) 'ℚ.nz)
      (add-op '_÷ (list 'ℚ.nn 'ℚ.p) 'ℚ.nn)
      (add-op '_÷ (list 'ℚ.p 'ℚ.p) 'ℚ.p)
      (add-op '^ (list 'ℚ.nz 'ℤ.nz) 'ℚ.nz)
      (add-op '^ (list 'ℚ.p 'ℤ.nz) 'ℚ.p)
      (add-op '^ (list 'ℚ.nz 'zero) 'ℕ.nz)
      (add-op 'abs (list 'ℚ) 'ℚ.nn)
      (add-op 'abs (list 'ℚ.nz) 'ℚ.p)
      (add-op '_= (list 'ℚ 'ℚ) 'boolean)
      (add-op '_< (list 'ℚ 'ℚ) 'boolean)
      (add-op '_> (list 'ℚ 'ℚ) 'boolean)
      (add-op '_≤ (list 'ℚ 'ℚ) 'boolean)
      (add-op '_≥ (list 'ℚ 'ℚ) 'boolean)))

(module+ test
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_+ (list 1/2 2/3)) 'ℚ.p)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_+ (list 1/2 -2/3)) 'ℚ)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_- (list 1/2 -2/3)) 'ℚ)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '- (list -2/3)) 'ℚ)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_× (list 1/2 2/3)) 'ℚ.p)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_× (list 0 2/3)) 'ℚ.nn)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_× (list 1/2 -2/3)) 'ℚ.nz)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_÷ (list 1/2 -2/3)) 'ℚ.nz)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_÷ (list 1/2 2/3)) 'ℚ.p)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_÷ (list 1/2 0))
                (kind rational-sorts 'ℚ))
  (check-equal? (sort-of-numarg-term rational-signature
                                     '^ (list 1/2 2)) 'ℚ.p)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '^ (list 1/2 -2)) 'ℚ.p)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '^ (list -1/2 2)) 'ℚ.nz)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '^ (list -1/2 -2)) 'ℚ.nz)
  (check-equal? (sort-of-numarg-term rational-signature '^ (list 1/2 0))
                (number-term.sort 1))
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_= (list 1/2 2/3)) 'boolean)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_< (list 1/2 2/3)) 'boolean)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_> (list 1/2 2/3)) 'boolean)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_≤ (list 1/2 2/3)) 'boolean)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_≥ (list 1/2 2/3)) 'boolean)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_== (list 1/2 2/3)) 'boolean)
  (check-false (non-regular-op-example rational-signature)))

;
; IEEE binary floating-point formats
;
(define IEEE-float-sorts
  (~> empty-sort-graph
      (add-sort 'FP)
      (add-sort 'FP.number)
      (add-sort 'FP.NaN)
      (add-sort 'FP.inf)
      (add-subsort-relation 'FP.number 'FP)
      (add-subsort-relation 'FP.NaN 'FP)
      (add-subsort-relation 'FP.inf 'FP)

      (add-sort 'FP32)
      (add-sort 'FP32-number)
      (add-sort 'FP32-NaN)
      (add-sort 'FP32-inf)
      (add-subsort-relation 'FP32 'FP)
      (add-subsort-relation 'FP32-number 'FP32)
      (add-subsort-relation 'FP32-number 'FP.number)
      (add-subsort-relation 'FP32-NaN 'FP32)
      (add-subsort-relation 'FP32-NaN 'FP.NaN)
      (add-subsort-relation 'FP32-inf 'FP32)
      (add-subsort-relation 'FP32-inf 'FP.inf)

      (add-sort 'FP64)
      (add-sort 'FP64-number)
      (add-sort 'FP64-NaN)
      (add-sort 'FP64-inf)
      (add-subsort-relation 'FP64 'FP)
      (add-subsort-relation 'FP64-number 'FP64)
      (add-subsort-relation 'FP64-number 'FP.number)
      (add-subsort-relation 'FP64-NaN 'FP64)
      (add-subsort-relation 'FP64-NaN 'FP.NaN)
      (add-subsort-relation 'FP64-inf 'FP64)
      (add-subsort-relation 'FP64-inf 'FP.inf)))

(define IEEE-float-signature
  (~> (empty-signature IEEE-float-sorts #:builtins (set '*IEEE-floating-point*))
      (merge-signatures integer-signature #f)
      (add-op '_+ (list 'FP32 'FP32) 'FP32)
      (add-op '_- (list 'FP32 'FP32) 'FP32)
      (add-op '- (list 'FP32) 'FP32)
      (add-op '_× (list 'FP32 'FP32) 'FP32)
      (add-op '_÷ (list 'FP32 'FP32) 'FP32)
      (add-op '^ (list 'FP32 'FP32) 'FP32)
      (add-op '^ (list 'FP32 'ℤ) 'FP32)
      (add-op 'abs (list 'FP32) 'FP32)
      (add-op '√ (list 'FP32) 'FP32)
      (add-op '_= (list 'FP32 'FP32) 'boolean)
      (add-op '_< (list 'FP32 'FP32) 'boolean)
      (add-op '_> (list 'FP32 'FP32) 'boolean)
      (add-op '_≤ (list 'FP32 'FP32) 'boolean)
      (add-op '_≥ (list 'FP32 'FP32) 'boolean)
      (add-op '_+ (list 'FP64 'FP64) 'FP64)
      (add-op '_- (list 'FP64 'FP64) 'FP64)
      (add-op '- (list 'FP64) 'FP64)
      (add-op '_× (list 'FP64 'FP64) 'FP64)
      (add-op '_÷ (list 'FP64 'FP64) 'FP64)
      (add-op '^ (list 'FP64 'FP64) 'FP64)
      (add-op '^ (list 'FP64 'ℤ) 'FP64)
      (add-op 'abs (list 'FP64) 'FP64)
      (add-op '√ (list 'FP64) 'FP64)
      (add-op '_= (list 'FP64 'FP64) 'boolean)
      (add-op '_< (list 'FP64 'FP64) 'boolean)
      (add-op '_> (list 'FP64 'FP64) 'boolean)
      (add-op '_≤ (list 'FP64 'FP64) 'boolean)
      (add-op '_≥ (list 'FP64 'FP64) 'boolean)))

(module+ test
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_+ (list #x1s1 #x3s1)) 'FP32)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_+ (list #x1l1 #x3l1)) 'FP64)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_+ (list #x1s1 #x3l1))
                (kind IEEE-float-sorts 'FP32))
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_- (list #x1s1 #x3s1)) 'FP32)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '- (list #x3s1)) 'FP32)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_- (list #x1l1 #x3l1)) 'FP64)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '- (list #x3l1)) 'FP64)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_- (list #x1s1 #x3l1))
                (kind IEEE-float-sorts 'FP32))
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_× (list #x1s1 #x3s1)) 'FP32)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_× (list #x1l1 #x3l1)) 'FP64)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_× (list #x1s1 #x3l1))
                (kind IEEE-float-sorts 'FP32))
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_÷ (list #x1s1 #x3s1)) 'FP32)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_÷ (list #x1l1 #x3l1)) 'FP64)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_÷ (list #x1s1 #x3l1))
                (kind IEEE-float-sorts 'FP32))
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_= (list #x1s1 #x3s1)) 'boolean)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_< (list #x1s1 #x3s1)) 'boolean)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_> (list #x1s1 #x3s1)) 'boolean)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_≤ (list #x1s1 #x3s1)) 'boolean)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_≥ (list #x1s1 #x3s1)) 'boolean)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_== (list #x1s1 #x3s1)) 'boolean)
  (check-false (non-regular-op-example IEEE-float-signature)))

;
; Functions common to all number types
;
(define (number-term.sort x)
  (cond
    [(single-flonum? x)
     (case x
       [(+nan.f) 'FP32-NaN]
       [(+inf.f -inf.f) 'FP32-inf]
       [else 'FP32-number])]
    [(double-flonum? x)
     (case x
       [(+nan.0) 'FP64-NaN]
       [(+inf.0 -inf.0) 'FP64-inf]
       [else 'FP64-number])]
    [(inexact? x) (error "not supported")]
    [(zero? x) 'zero]
    [(integer? x) (if (positive? x) 'ℕ.nz 'ℤ.nz)]
    [else (if (positive? x) 'ℚ.p 'ℚ.nz)]))

(define (number-term.builtin-type x)
  (cond
    [(single-flonum? x) '*IEEE-floating-point*]
    [(double-flonum? x) '*IEEE-floating-point*]
    [(inexact? x) (error "not supported")]
    [(integer? x) '*integer*]
    [else '*rational*]))
