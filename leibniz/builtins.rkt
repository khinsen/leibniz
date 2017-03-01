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
; ℕ\0  non-zero natural
; ℤ    integer
; ℤ\0  non-zero integer
;
; "truth" is included because comparisons have sort boolean.
;
(define integer-sorts
  (~> truth-sorts
      ; Natural numbers
      (add-sort 'ℕ)
      (add-sort 'zero)
      (add-subsort-relation 'zero 'ℕ)
      (add-sort 'ℕ\\0)
      (add-subsort-relation 'ℕ\\0 'ℕ)
      ; Integers
      (add-sort 'ℤ)
      (add-subsort-relation 'ℕ 'ℤ)
      (add-sort 'ℤ\\0)
      (add-subsort-relation 'ℤ\\0 'ℤ)
      (add-subsort-relation 'ℕ\\0 'ℤ\\0)))

(define integer-signature
  (~> (empty-signature integer-sorts #:builtins (set '*integer*))
      (merge-signatures truth-signature #f)
      (add-op '_+ (list 'ℤ 'ℤ) 'ℤ)
      (add-op '_+ (list 'ℕ 'ℕ) 'ℕ)
      (add-op '_+ (list 'ℕ\\0 'ℕ) 'ℕ\\0)
      (add-op '_+ (list 'ℕ 'ℕ\\0) 'ℕ\\0)
      (add-op '_- (list 'ℤ 'ℤ) 'ℤ)
      (add-op '_× (list 'ℤ 'ℤ) 'ℤ)
      (add-op '_× (list 'ℕ 'ℕ) 'ℕ)
      (add-op '_× (list 'ℕ\\0 'ℕ\\0) 'ℕ\\0)
      (add-op '_× (list 'ℤ\\0 'ℤ\\0) 'ℤ\\0)
      (add-op '_div (list 'ℤ 'ℤ\\0) 'ℤ)
      (add-op '_div (list 'ℕ 'ℕ\\0) 'ℕ)
      (add-op '_rem (list 'ℤ 'ℤ\\0) 'ℤ)
      (add-op '_rem (list 'ℕ 'ℕ\\0) 'ℕ)
      (add-op '^ (list 'ℤ 'ℕ\\0) 'ℤ)
      (add-op '^ (list 'ℕ 'ℕ\\0) 'ℕ)
      (add-op '^ (list 'ℕ\\0 'ℕ\\0) 'ℕ\\0)
      (add-op '^ (list 'ℤ\\0 'ℕ\\0) 'ℤ\\0)
      (add-op '^ (list 'ℤ\\0 'zero) 'ℕ\\0)
      (add-op 'abs (list 'ℤ) 'ℕ)
      (add-op 'abs (list 'ℤ\\0) 'ℕ\\0)
      (add-op '_< (list 'ℤ 'ℤ) 'boolean)
      (add-op '_> (list 'ℤ 'ℤ) 'boolean)
      (add-op '_≤ (list 'ℤ 'ℤ) 'boolean)
      (add-op '_≥ (list 'ℤ 'ℤ) 'boolean)))

(module+ test
  (check-equal? (sort-of-numarg-term integer-signature '_+ (list 1 2)) 'ℕ\\0)
  (check-equal? (sort-of-numarg-term integer-signature '_+ (list 0 2)) 'ℕ\\0)
  (check-equal? (sort-of-numarg-term integer-signature '_+ (list 0 0)) 'ℕ)
  (check-equal? (sort-of-numarg-term integer-signature '_+ (list -1 0)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_- (list 0 0)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_× (list 1 2)) 'ℕ\\0)
  (check-equal? (sort-of-numarg-term integer-signature '_× (list 0 2)) 'ℕ)
  (check-equal? (sort-of-numarg-term integer-signature '_× (list -2 0)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_× (list -2 -2)) 'ℤ\\0)
  (check-equal? (sort-of-numarg-term integer-signature '_div (list -2 -2)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_div (list 0 1)) 'ℕ)
  (check-equal? (sort-of-numarg-term integer-signature '_rem (list -2 -2)) 'ℤ)
  (check-equal? (sort-of-numarg-term integer-signature '_rem (list 0 1)) 'ℕ)
  (check-equal? (sort-of-numarg-term integer-signature '^ (list 2 1)) 'ℕ\\0)
  (check-equal? (sort-of-numarg-term integer-signature '^ (list 2 0))
                (number-term.sort 1))
  (check-equal? (sort-of-numarg-term integer-signature '^ (list -2 1)) 'ℤ\\0)
  (check-equal? (sort-of-numarg-term integer-signature '^ (list -2 0))
                (number-term.sort 1))
  (check-equal? (sort-of-numarg-term integer-signature '^ (list 0 0))
                (kind integer-sorts 'ℤ))
  (check-equal? (sort-of-numarg-term integer-signature '_< (list 0 1)) 'boolean)
  (check-equal? (sort-of-numarg-term integer-signature '_> (list 0 1)) 'boolean)
  (check-equal? (sort-of-numarg-term integer-signature '_≤ (list 0 1)) 'boolean)
  (check-equal? (sort-of-numarg-term integer-signature '_≥ (list 0 1)) 'boolean)
  (check-equal? (sort-of-numarg-term integer-signature '== (list 0 1)) 'boolean)
  (check-false (non-regular-op-example integer-signature)))

;
; Rationals and their subsets
; 
; ℚ    rational
; ℚ\0  non-zero rational
; ℚ+   positive rational
; ℚ+0  non-negative rational
;
(define rational-sorts
  (~> integer-sorts
      ; Rational numbers
      (add-sort 'ℚ)
      (add-subsort-relation 'ℤ 'ℚ)
      (add-sort 'ℚ\\0)
      (add-subsort-relation 'ℚ\\0 'ℚ)
      (add-subsort-relation 'ℤ\\0 'ℚ\\0)
      (add-sort 'ℚ+)
      (add-subsort-relation 'ℚ+ 'ℚ\\0)
      (add-subsort-relation 'ℕ\\0 'ℚ+)
      (add-sort 'ℚ+0)
      (add-subsort-relation 'ℚ+0 'ℚ)
      (add-subsort-relation 'ℚ+ 'ℚ+0)
      (add-subsort-relation 'ℕ 'ℚ+0)))

(define rational-signature
  (~> (empty-signature rational-sorts #:builtins (set '*rational*))
      (merge-signatures integer-signature #f)
      (add-op '_+ (list 'ℚ 'ℚ) 'ℚ)
      (add-op '_+ (list 'ℚ+ 'ℚ+) 'ℚ+)
      (add-op '_+ (list 'ℚ+0 'ℚ+0) 'ℚ+0)
      (add-op '_- (list 'ℚ 'ℚ) 'ℚ)
      (add-op '_× (list 'ℚ 'ℚ) 'ℚ)
      (add-op '_× (list 'ℚ+ 'ℚ+) 'ℚ+)
      (add-op '_× (list 'ℚ+0 'ℚ+0) 'ℚ+0)
      (add-op '_× (list 'ℚ\\0 'ℚ\\0) 'ℚ\\0)
      (add-op '_÷ (list 'ℚ 'ℚ\\0) 'ℚ)
      (add-op '_÷ (list 'ℚ\\0 'ℚ\\0) 'ℚ\\0)
      (add-op '_÷ (list 'ℚ+0 'ℚ+) 'ℚ+0)
      (add-op '_÷ (list 'ℚ+ 'ℚ+) 'ℚ+)
      (add-op '^ (list 'ℚ\\0 'ℤ\\0) 'ℚ\\0)
      (add-op '^ (list 'ℚ+ 'ℤ\\0) 'ℚ+)
      (add-op '^ (list 'ℚ\\0 'zero) 'ℕ\\0)
      (add-op 'abs (list 'ℚ) 'ℚ+0)
      (add-op 'abs (list 'ℚ\\0) 'ℚ+)
      (add-op '_< (list 'ℚ 'ℚ) 'boolean)
      (add-op '_> (list 'ℚ 'ℚ) 'boolean)
      (add-op '_≤ (list 'ℚ 'ℚ) 'boolean)
      (add-op '_≥ (list 'ℚ 'ℚ) 'boolean)))

(module+ test
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_+ (list 1/2 2/3)) 'ℚ+)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_+ (list 1/2 -2/3)) 'ℚ)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_- (list 1/2 -2/3)) 'ℚ)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_× (list 1/2 2/3)) 'ℚ+)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_× (list 0 2/3)) 'ℚ+0)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_× (list 1/2 -2/3)) 'ℚ\\0)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_÷ (list 1/2 -2/3)) 'ℚ\\0)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_÷ (list 1/2 2/3)) 'ℚ+)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_÷ (list 1/2 0))
                (kind rational-sorts 'ℚ))
  (check-equal? (sort-of-numarg-term rational-signature
                                     '^ (list 1/2 2)) 'ℚ+)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '^ (list 1/2 -2)) 'ℚ+)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '^ (list -1/2 2)) 'ℚ\\0)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '^ (list -1/2 -2)) 'ℚ\\0)
  (check-equal? (sort-of-numarg-term rational-signature '^ (list 1/2 0))
                (number-term.sort 1))
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_< (list 1/2 2/3)) 'boolean)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_> (list 1/2 2/3)) 'boolean)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_≤ (list 1/2 2/3)) 'boolean)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '_≥ (list 1/2 2/3)) 'boolean)
  (check-equal? (sort-of-numarg-term rational-signature
                                     '== (list 1/2 2/3)) 'boolean)
  (check-false (non-regular-op-example rational-signature)))

;
; IEEE binary floating-point formats
;
(define IEEE-float-sorts
  (~> empty-sort-graph
      (add-sort 'IEEE-floating-point)
      (add-sort 'IEEE-binary32)
      (add-subsort-relation 'IEEE-binary32 'IEEE-floating-point)
      (add-sort 'IEEE-binary64)
      (add-subsort-relation 'IEEE-binary64 'IEEE-floating-point)))

(define IEEE-float-signature
  (~> (empty-signature IEEE-float-sorts #:builtins (set '*IEEE-floating-point*))
      (merge-signatures truth-signature #f)
      (add-op '_+ (list 'IEEE-binary32 'IEEE-binary32) 'IEEE-binary32)
      (add-op '_- (list 'IEEE-binary32 'IEEE-binary32) 'IEEE-binary32)
      (add-op '_× (list 'IEEE-binary32 'IEEE-binary32) 'IEEE-binary32)
      (add-op '_÷ (list 'IEEE-binary32 'IEEE-binary32) 'IEEE-binary32)
      (add-op '^ (list 'IEEE-binary32 'IEEE-binary32) 'IEEE-binary32)
      (add-op 'abs (list 'IEEE-binary32) 'IEEE-binary32)
      (add-op '_< (list 'IEEE-binary32 'IEEE-binary32) 'boolean)
      (add-op '_> (list 'IEEE-binary32 'IEEE-binary32) 'boolean)
      (add-op '_≤ (list 'IEEE-binary32 'IEEE-binary32) 'boolean)
      (add-op '_≥ (list 'IEEE-binary32 'IEEE-binary32) 'boolean)
      (add-op '_+ (list 'IEEE-binary64 'IEEE-binary64) 'IEEE-binary64)
      (add-op '_- (list 'IEEE-binary64 'IEEE-binary64) 'IEEE-binary64)
      (add-op '_× (list 'IEEE-binary64 'IEEE-binary64) 'IEEE-binary64)
      (add-op '_÷ (list 'IEEE-binary64 'IEEE-binary64) 'IEEE-binary64)
      (add-op '^ (list 'IEEE-binary64 'IEEE-binary64) 'IEEE-binary64)
      (add-op 'abs (list 'IEEE-binary64) 'IEEE-binary64)
      (add-op '√ (list 'IEEE-binary64) 'IEEE-binary64)
      (add-op '_< (list 'IEEE-binary64 'IEEE-binary64) 'boolean)
      (add-op '_> (list 'IEEE-binary64 'IEEE-binary64) 'boolean)
      (add-op '_≤ (list 'IEEE-binary64 'IEEE-binary64) 'boolean)
      (add-op '_≥ (list 'IEEE-binary64 'IEEE-binary64) 'boolean)))

(module+ test
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_+ (list #x1s1 #x3s1)) 'IEEE-binary32)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_+ (list #x1l1 #x3l1)) 'IEEE-binary64)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_+ (list #x1s1 #x3l1))
                (kind IEEE-float-sorts 'IEEE-binary32))
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_- (list #x1s1 #x3s1)) 'IEEE-binary32)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_- (list #x1l1 #x3l1)) 'IEEE-binary64)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_- (list #x1s1 #x3l1))
                (kind IEEE-float-sorts 'IEEE-binary32))
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_× (list #x1s1 #x3s1)) 'IEEE-binary32)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_× (list #x1l1 #x3l1)) 'IEEE-binary64)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_× (list #x1s1 #x3l1))
                (kind IEEE-float-sorts 'IEEE-binary32))
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_÷ (list #x1s1 #x3s1)) 'IEEE-binary32)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_÷ (list #x1l1 #x3l1)) 'IEEE-binary64)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_÷ (list #x1s1 #x3l1))
                (kind IEEE-float-sorts 'IEEE-binary32))
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_< (list #x1s1 #x3s1)) 'boolean)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_> (list #x1s1 #x3s1)) 'boolean)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_≤ (list #x1s1 #x3s1)) 'boolean)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '_≥ (list #x1s1 #x3s1)) 'boolean)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '== (list #x1s1 #x3s1)) 'boolean)
  (check-false (non-regular-op-example IEEE-float-signature)))

;
; Functions common to all number types
;
(define (number-term.sort x)
  (cond
    [(single-flonum? x) 'IEEE-binary32]
    [(double-flonum? x) 'IEEE-binary64]
    [(inexact? x) (error "not supported")]
    [(zero? x) 'zero]
    [(integer? x) (if (positive? x) 'ℕ\\0 'ℤ\\0)]
    [else (if (positive? x) 'ℚ+ 'ℚ\\0)]))

(define (number-term.builtin-type x)
  (cond
    [(single-flonum? x) '*IEEE-floating-point*]
    [(double-flonum? x) '*IEEE-floating-point*]
    [(inexact? x) (error "not supported")]
    [(integer? x) '*integer*]
    [else '*rational*]))
