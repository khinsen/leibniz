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
  (empty-signature string-sorts #:builtins (set '*string*)))

;
; Integers and their subsets
;
; ℕ    natural
; ℕnz  non-zero natural
; ℤ    integer
; ℤnz  non-zero integer
;
; "truth" is included because comparisons have sort boolean.
;
(define integer-sorts
  (~> truth-sorts
      ; Natural numbers
      (add-sort 'ℕ)
      (add-sort 'zero)
      (add-subsort-relation 'zero 'ℕ)
      (add-sort 'ℕnz)
      (add-subsort-relation 'ℕnz 'ℕ)
      ; Integers
      (add-sort 'ℤ)
      (add-subsort-relation 'ℕ 'ℤ)
      (add-sort 'ℤnz)
      (add-subsort-relation 'ℤnz 'ℤ)
      (add-subsort-relation 'ℕnz 'ℤnz)))

(define integer-signature
  (~> (empty-signature integer-sorts #:builtins (set '*integer*))
      (merge-signatures truth-signature #f)
      (add-op '_+ (list 'ℤ 'ℤ) 'ℤ)
      (add-op '_+ (list 'ℕ 'ℕ) 'ℕ)
      (add-op '_+ (list 'ℕnz 'ℕ) 'ℕnz)
      (add-op '_+ (list 'ℕ 'ℕnz) 'ℕnz)
      (add-op '_- (list 'ℤ 'ℤ) 'ℤ)
      (add-op '- (list 'ℤ) 'ℤ)
      (add-op '_× (list 'ℤ 'ℤ) 'ℤ)
      (add-op '_× (list 'ℕ 'ℕ) 'ℕ)
      (add-op '_× (list 'ℕnz 'ℕnz) 'ℕnz)
      (add-op '_× (list 'ℤnz 'ℤnz) 'ℤnz)
      (add-op '_div (list 'ℤ 'ℤnz) 'ℤ)
      (add-op '_div (list 'ℕ 'ℕnz) 'ℕ)
      (add-op '_rem (list 'ℤ 'ℤnz) 'ℤ)
      (add-op '_rem (list 'ℕ 'ℕnz) 'ℕ)
      (add-op '^ (list 'ℤ 'ℕnz) 'ℤ)
      (add-op '^ (list 'ℕ 'ℕnz) 'ℕ)
      (add-op '^ (list 'ℕnz 'ℕnz) 'ℕnz)
      (add-op '^ (list 'ℤnz 'ℕnz) 'ℤnz)
      (add-op '^ (list 'ℤnz 'zero) 'ℕnz)
      (add-op 'abs (list 'ℤ) 'ℕ)
      (add-op 'abs (list 'ℤnz) 'ℕnz)
      (add-op '_= (list 'ℤ 'ℤ) 'boolean)
      (add-op '_< (list 'ℤ 'ℤ) 'boolean)
      (add-op '_> (list 'ℤ 'ℤ) 'boolean)
      (add-op '_≤ (list 'ℤ 'ℤ) 'boolean)
      (add-op '_≥ (list 'ℤ 'ℤ) 'boolean)))

(module+ test
  (check-equal? (sort-of-numarg-term integer-signature '_+ (list 1 2)) 'ℕnz)
  (check-equal? (sort-of-numarg-term integer-signature '_+ (list 0 2)) 'ℕnz)
  (check-equal? (sort-of-numarg-term integer-signature '_+ (list 0 0)) 'ℕ)
  (check-equal? (sort-of-numarg-term integer-signature '_+ (list -1 0)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_- (list 0 0)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '- (list 1)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_× (list 1 2)) 'ℕnz)
  (check-equal? (sort-of-numarg-term integer-signature '_× (list 0 2)) 'ℕ)
  (check-equal? (sort-of-numarg-term integer-signature '_× (list -2 0)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_× (list -2 -2)) 'ℤnz)
  (check-equal? (sort-of-numarg-term integer-signature '_div (list -2 -2)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_div (list 0 1)) 'ℕ)
  (check-equal? (sort-of-numarg-term integer-signature '_rem (list -2 -2)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_rem (list 0 1)) 'ℕ)
  (check-equal? (sort-of-numarg-term integer-signature '^ (list 2 1)) 'ℕnz)
  (check-equal? (sort-of-numarg-term integer-signature '^ (list 2 0))
                (number-term.sort 1))
  (check-equal? (sort-of-numarg-term integer-signature '^ (list -2 1)) 'ℤnz)
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
; ℚ    rational
; ℚnz  non-zero rational
; ℚp   positive rational
; ℚnn  non-negative rational
;
(define rational-sorts
  (~> integer-sorts
      ; Rational numbers
      (add-sort 'ℚ)
      (add-subsort-relation 'ℤ 'ℚ)
      (add-sort 'ℚnz)
      (add-subsort-relation 'ℚnz 'ℚ)
      (add-subsort-relation 'ℤnz 'ℚnz)
      (add-sort 'ℚp)
      (add-subsort-relation 'ℚp 'ℚnz)
      (add-subsort-relation 'ℕnz 'ℚp)
      (add-sort 'ℚnn)
      (add-subsort-relation 'ℚnn 'ℚ)
      (add-subsort-relation 'ℚp 'ℚnn)
      (add-subsort-relation 'ℕ 'ℚnn)))

(define rational-signature
  (~> (empty-signature rational-sorts #:builtins (set '*rational*))
      (merge-signatures integer-signature #f)
      (add-op '_+ (list 'ℚ 'ℚ) 'ℚ)
      (add-op '_+ (list 'ℚp 'ℚp) 'ℚp)
      (add-op '_+ (list 'ℚnn 'ℚnn) 'ℚnn)
      (add-op '_- (list 'ℚ 'ℚ) 'ℚ)
      (add-op '- (list 'ℚ) 'ℚ)
      (add-op '_× (list 'ℚ 'ℚ) 'ℚ)
      (add-op '_× (list 'ℚp 'ℚp) 'ℚp)
      (add-op '_× (list 'ℚnn 'ℚnn) 'ℚnn)
      (add-op '_× (list 'ℚnz 'ℚnz) 'ℚnz)
      (add-op '_÷ (list 'ℚ 'ℚnz) 'ℚ)
      (add-op '_÷ (list 'ℚnz 'ℚnz) 'ℚnz)
      (add-op '_÷ (list 'ℚnn 'ℚp) 'ℚnn)
      (add-op '_÷ (list 'ℚp 'ℚp) 'ℚp)
      (add-op '^ (list 'ℚnz 'ℤnz) 'ℚnz)
      (add-op '^ (list 'ℚp 'ℤnz) 'ℚp)
      (add-op '^ (list 'ℚnz 'zero) 'ℕnz)
      (add-op 'abs (list 'ℚ) 'ℚnn)
      (add-op 'abs (list 'ℚnz) 'ℚp)
      (add-op '_= (list 'ℚ 'ℚ) 'boolean)
      (add-op '_< (list 'ℚ 'ℚ) 'boolean)
      (add-op '_> (list 'ℚ 'ℚ) 'boolean)
      (add-op '_≤ (list 'ℚ 'ℚ) 'boolean)
      (add-op '_≥ (list 'ℚ 'ℚ) 'boolean)))

(module+ test
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_+ (list 1/2 2/3)) 'ℚp)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_+ (list 1/2 -2/3)) 'ℚ)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_- (list 1/2 -2/3)) 'ℚ)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '- (list -2/3)) 'ℚ)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_× (list 1/2 2/3)) 'ℚp)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_× (list 0 2/3)) 'ℚnn)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_× (list 1/2 -2/3)) 'ℚnz)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_÷ (list 1/2 -2/3)) 'ℚnz)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_÷ (list 1/2 2/3)) 'ℚp)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_÷ (list 1/2 0))
                (kind rational-sorts 'ℚ))
  (check-equal? (sort-of-numarg-term rational-signature
                                     '^ (list 1/2 2)) 'ℚp)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '^ (list 1/2 -2)) 'ℚp)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '^ (list -1/2 2)) 'ℚnz)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '^ (list -1/2 -2)) 'ℚnz)
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
      (add-sort 'FP-number)
      (add-sort 'FP-NaN)
      (add-sort 'FP-inf)
      (add-subsort-relation 'FP-number 'FP)
      (add-subsort-relation 'FP-NaN 'FP)
      (add-subsort-relation 'FP-inf 'FP)

      (add-sort 'FP32)
      (add-sort 'FP32-number)
      (add-sort 'FP32-NaN)
      (add-sort 'FP32-inf)
      (add-subsort-relation 'FP32 'FP)
      (add-subsort-relation 'FP32-number 'FP32)
      (add-subsort-relation 'FP32-number 'FP-number)
      (add-subsort-relation 'FP32-NaN 'FP32)
      (add-subsort-relation 'FP32-NaN 'FP-NaN)
      (add-subsort-relation 'FP32-inf 'FP32)
      (add-subsort-relation 'FP32-inf 'FP-inf)

      (add-sort 'FP64)
      (add-sort 'FP64-number)
      (add-sort 'FP64-NaN)
      (add-sort 'FP64-inf)
      (add-subsort-relation 'FP64 'FP)
      (add-subsort-relation 'FP64-number 'FP64)
      (add-subsort-relation 'FP64-number 'FP-number)
      (add-subsort-relation 'FP64-NaN 'FP64)
      (add-subsort-relation 'FP64-NaN 'FP-NaN)
      (add-subsort-relation 'FP64-inf 'FP64)
      (add-subsort-relation 'FP64-inf 'FP-inf)))

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
    [(integer? x) (if (positive? x) 'ℕnz 'ℤnz)]
    [else (if (positive? x) 'ℚp 'ℚnz)]))

(define (number-term.builtin-type x)
  (cond
    [(single-flonum? x) '*IEEE-floating-point*]
    [(double-flonum? x) '*IEEE-floating-point*]
    [(inexact? x) (error "not supported")]
    [(integer? x) '*integer*]
    [else '*rational*]))
