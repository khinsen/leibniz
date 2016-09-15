#lang racket

(provide number-term.sort number-term.builtin-type
         truth-sorts truth-signature
         symbol-sorts symbol-signature
         string-sorts string-signature
         integer-sorts integer-signature
         exact-number-sorts exact-number-signature
         IEEE-float-sorts IEEE-float-signature)

(require "./sorts.rkt"
         "./operators.rkt"
         rackjure/threading)

(module+ test
  (require rackunit racket/function rackjure/threading)
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
      (add-sort 'Boolean)))

(define truth-signature
  (~> (empty-signature truth-sorts #:builtins (set '*truth*))
      (add-op 'true empty 'Boolean)
      (add-op 'false empty 'Boolean)))

(module+ test
  (check-equal? (sort-of-noarg-term truth-signature 'true) 'Boolean)
  (check-equal? (sort-of-noarg-term truth-signature 'false) 'Boolean))

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
      (add-sort 'Symbol)))

(define symbol-signature
  (empty-signature symbol-sorts #:builtins (set '*symbol*)))

;
; Strings
;
(define string-sorts
  (~> empty-sort-graph
      (add-sort 'String)))

(define string-signature
  (empty-signature string-sorts #:builtins (set '*string*)))

;
; Integers and their subsets
;
; "truth" is included because comparisons have sort Boolean.
;
(define integer-sorts
  (~> truth-sorts
      ; Natural numbers
      (add-sort 'Natural)
      (add-sort 'Zero)
      (add-subsort-relation 'Zero 'Natural)
      (add-sort 'NonZeroNatural)
      (add-subsort-relation 'NonZeroNatural 'Natural)
      ; Integers
      (add-sort 'Integer)
      (add-subsort-relation 'Natural 'Integer)
      (add-sort 'NonZeroInteger)
      (add-subsort-relation 'NonZeroInteger 'Integer)
      (add-subsort-relation 'NonZeroNatural 'NonZeroInteger)))

(define integer-signature
  (~> (empty-signature integer-sorts #:builtins (set '*integer*))
      (merge-signatures truth-signature)
      (add-op '+ (list 'Integer 'Integer) 'Integer)
      (add-op '+ (list 'Natural 'Natural) 'Natural)
      (add-op '+ (list 'NonZeroNatural 'Natural) 'NonZeroNatural)
      (add-op '+ (list 'Natural 'NonZeroNatural) 'NonZeroNatural)
      (add-op '+ (list 'Zero 'Zero) 'Zero)
      (add-op '- (list 'Integer 'Integer) 'Integer)
      (add-op '* (list 'Integer 'Integer) 'Integer)
      (add-op '* (list 'Integer 'Zero) 'Zero)
      (add-op '* (list 'Zero 'Integer) 'Zero)
      (add-op '* (list 'Natural 'Natural) 'Natural)
      (add-op '* (list 'NonZeroNatural 'NonZeroNatural) 'NonZeroNatural)
      (add-op 'div (list 'Integer 'NonZeroInteger) 'Integer)
      (add-op 'div (list 'Zero 'NonZeroInteger) 'Zero)
      (add-op 'div (list 'Natural 'NonZeroNatural) 'Natural)
      (add-op 'rem (list 'Integer 'NonZeroInteger) 'Integer)
      (add-op 'rem (list 'Zero 'NonZeroInteger) 'Zero)
      (add-op 'rem (list 'Natural 'NonZeroNatural) 'Natural)
      (add-op '< (list 'Integer 'Integer) 'Boolean)
      (add-op '> (list 'Integer 'Integer) 'Boolean)
      (add-op '<= (list 'Integer 'Integer) 'Boolean)
      (add-op '>= (list 'Integer 'Integer) 'Boolean)))

(module+ test
  (check-equal? (sort-of-numarg-term integer-signature '+ (list 1 2)) 'NonZeroNatural)
  (check-equal? (sort-of-numarg-term integer-signature '+ (list 0 2)) 'NonZeroNatural)
  (check-equal? (sort-of-numarg-term integer-signature '+ (list 0 0)) 'Zero)
  (check-equal? (sort-of-numarg-term integer-signature '+ (list -1 0)) 'Integer)
  (check-equal? (sort-of-numarg-term integer-signature '- (list 0 0)) 'Integer)
  (check-equal? (sort-of-numarg-term integer-signature '* (list 1 2)) 'NonZeroNatural)
  (check-equal? (sort-of-numarg-term integer-signature '* (list 0 2)) 'Zero)
  (check-equal? (sort-of-numarg-term integer-signature '* (list -2 0)) 'Zero)
  (check-equal? (sort-of-numarg-term integer-signature '* (list -2 -2)) 'Integer)
  (check-equal? (sort-of-numarg-term integer-signature 'div (list -2 -2)) 'Integer)
  (check-equal? (sort-of-numarg-term integer-signature 'div (list 0 1)) 'Zero)
  (check-equal? (sort-of-numarg-term integer-signature 'rem (list -2 -2)) 'Integer)
  (check-equal? (sort-of-numarg-term integer-signature 'rem (list 0 1)) 'Zero)
  (check-equal? (sort-of-numarg-term integer-signature '< (list 0 1)) 'Boolean)
  (check-equal? (sort-of-numarg-term integer-signature '> (list 0 1)) 'Boolean)
  (check-equal? (sort-of-numarg-term integer-signature '<= (list 0 1)) 'Boolean)
  (check-equal? (sort-of-numarg-term integer-signature '>= (list 0 1)) 'Boolean)
  (check-equal? (sort-of-numarg-term integer-signature '== (list 0 1)) 'Boolean))

;
; Rationals and their subsets
;
(define exact-number-sorts
  (~> integer-sorts
      ; Rational numbers
      (add-sort 'Rational)
      (add-subsort-relation 'Integer 'Rational)
      (add-sort 'NonZeroRational)
      (add-subsort-relation 'NonZeroRational 'Rational)
      (add-subsort-relation 'NonZeroInteger 'NonZeroRational)
      (add-sort 'PositiveRational)
      (add-subsort-relation 'PositiveRational 'NonZeroRational)
      (add-subsort-relation 'NonZeroNatural 'PositiveRational)))

(define exact-number-signature
  (~> (empty-signature exact-number-sorts #:builtins (set '*rational*))
      (merge-signatures integer-signature)
      (add-op '+ (list 'Rational 'Rational) 'Rational)
      (add-op '+ (list 'PositiveRational 'PositiveRational) 'PositiveRational)
      (add-op '- (list 'Rational 'Rational) 'Rational)
      (add-op '* (list 'Rational 'Rational) 'Rational)
      (add-op '* (list 'PositiveRational 'PositiveRational) 'PositiveRational)
      (add-op '* (list 'NonZeroRational 'NonZeroRational) 'NonZeroRational)
      (add-op '* (list 'Zero 'Rational) 'Zero)
      (add-op '* (list 'Rational 'Zero) 'Zero)
      (add-op '/ (list 'Rational 'NonZeroRational) 'Rational)
      (add-op '/ (list 'PositiveRational 'PositiveRational) 'PositiveRational)
      (add-op '/ (list 'Zero 'NonZeroRational) 'Zero)
      (add-op '< (list 'Rational 'Rational) 'Boolean)
      (add-op '> (list 'Rational 'Rational) 'Boolean)
      (add-op '<= (list 'Rational 'Rational) 'Boolean)
      (add-op '>= (list 'Rational 'Rational) 'Boolean)))

(module+ test
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '+ (list 1/2 2/3)) 'PositiveRational)
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '+ (list 1/2 -2/3)) 'Rational)
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '- (list 1/2 -2/3)) 'Rational)
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '* (list 1/2 2/3)) 'PositiveRational)
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '* (list 0 2/3)) 'Zero)
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '* (list 1/2 -2/3)) 'NonZeroRational)
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '/ (list 1/2 -2/3)) 'Rational)
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '/ (list 1/2 2/3)) 'PositiveRational)
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '/ (list 1/2 0))
                (kind exact-number-sorts 'Rational))
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '< (list 1/2 2/3)) 'Boolean)
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '> (list 1/2 2/3)) 'Boolean)
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '<= (list 1/2 2/3)) 'Boolean)
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '>= (list 1/2 2/3)) 'Boolean)
  (check-equal? (sort-of-numarg-term exact-number-signature
                                     '== (list 1/2 2/3)) 'Boolean))

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
      (merge-signatures truth-signature)
      (add-op '+ (list 'IEEE-binary32 'IEEE-binary32) 'IEEE-binary32)
      (add-op '- (list 'IEEE-binary32 'IEEE-binary32) 'IEEE-binary32)
      (add-op '* (list 'IEEE-binary32 'IEEE-binary32) 'IEEE-binary32)
      (add-op '/ (list 'IEEE-binary32 'IEEE-binary32) 'IEEE-binary32)
      (add-op '< (list 'IEEE-binary32 'IEEE-binary32) 'Boolean)
      (add-op '> (list 'IEEE-binary32 'IEEE-binary32) 'Boolean)
      (add-op '<= (list 'IEEE-binary32 'IEEE-binary32) 'Boolean)
      (add-op '>= (list 'IEEE-binary32 'IEEE-binary32) 'Boolean)
      (add-op '+ (list 'IEEE-binary64 'IEEE-binary64) 'IEEE-binary64)
      (add-op '- (list 'IEEE-binary64 'IEEE-binary64) 'IEEE-binary64)
      (add-op '* (list 'IEEE-binary64 'IEEE-binary64) 'IEEE-binary64)
      (add-op '/ (list 'IEEE-binary64 'IEEE-binary64) 'IEEE-binary64)
      (add-op '< (list 'IEEE-binary64 'IEEE-binary64) 'Boolean)
      (add-op '> (list 'IEEE-binary64 'IEEE-binary64) 'Boolean)
      (add-op '<= (list 'IEEE-binary64 'IEEE-binary64) 'Boolean)
      (add-op '>= (list 'IEEE-binary64 'IEEE-binary64) 'Boolean)))

(module+ test
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '+ (list #x1s1 #x3s1)) 'IEEE-binary32)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '+ (list #x1l1 #x3l1)) 'IEEE-binary64)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '+ (list #x1s1 #x3l1))
                (kind IEEE-float-sorts 'IEEE-binary32))
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '- (list #x1s1 #x3s1)) 'IEEE-binary32)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '- (list #x1l1 #x3l1)) 'IEEE-binary64)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '- (list #x1s1 #x3l1))
                (kind IEEE-float-sorts 'IEEE-binary32))
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '* (list #x1s1 #x3s1)) 'IEEE-binary32)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '* (list #x1l1 #x3l1)) 'IEEE-binary64)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '* (list #x1s1 #x3l1))
                (kind IEEE-float-sorts 'IEEE-binary32))
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '/ (list #x1s1 #x3s1)) 'IEEE-binary32)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '/ (list #x1l1 #x3l1)) 'IEEE-binary64)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '/ (list #x1s1 #x3l1))
                (kind IEEE-float-sorts 'IEEE-binary32))
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '< (list #x1s1 #x3s1)) 'Boolean)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '> (list #x1s1 #x3s1)) 'Boolean)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '<= (list #x1s1 #x3s1)) 'Boolean)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '>= (list #x1s1 #x3s1)) 'Boolean)
  (check-equal? (sort-of-numarg-term IEEE-float-signature
                                     '== (list #x1s1 #x3s1)) 'Boolean))

;
; Functions common to all number types
;
(define (number-term.sort x)
  (cond
    [(single-flonum? x) 'IEEE-binary32]
    [(double-flonum? x) 'IEEE-binary64]
    [(inexact? x) (error "not supported")]
    [(zero? x) 'Zero]
    [(integer? x) (if (positive? x) 'NonZeroNatural 'NonZeroInteger)]
    [else (if (positive? x) 'PositiveRational 'NonZeroRational)]))

(define (number-term.builtin-type x)
  (cond
    [(single-flonum? x) '*IEEE-floating-point*]
    [(double-flonum? x) '*IEEE-floating-point*]
    [(inexact? x) (error "not supported")]
    [(integer? x) '*integer*]
    [else '*rational*]))
