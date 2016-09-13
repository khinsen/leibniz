#lang racket

(provide number-term.sort number-term.builtin-type
         truth-sorts truth-signature
         symbol-sorts symbol-signature
         string-sorts string-signature
         integer-sorts integer-signature
         exact-number-sorts exact-number-signature)

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
  (~> (empty-signature truth-sorts)
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
(define integer-sorts
  (~> empty-sort-graph
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
      (add-op 'rem (list 'Natural 'NonZeroNatural) 'Natural)))

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
  (check-equal? (sort-of-numarg-term integer-signature 'rem (list 0 1)) 'Zero))

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
  (~> (merge-signatures integer-signature
                        (empty-signature exact-number-sorts
                                         #:builtins (set '*rational*)))
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
      (add-op '/ (list 'Zero 'NonZeroRational) 'Zero)))

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
                (kind exact-number-sorts 'Rational)))

(define (number-term.sort x)
  (cond
    [(inexact? x) (error "not yet implemented")]
    [(zero? x) 'Zero]
    [(integer? x) (if (positive? x) 'NonZeroNatural 'NonZeroInteger)]
    [else (if (positive? x) 'PositiveRational 'NonZeroRational)]))

(define (number-term.builtin-type x)
  (cond
    [(inexact? x) (error "not yet implemented")]
    [(integer? x) '*integer*]
    [else '*rational*]))
