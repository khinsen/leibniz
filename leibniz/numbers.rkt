#lang racket

(provide number-term.sort
         integer-sorts
         exact-number-sorts)

(require "./sorts.rkt"
         rackjure/threading)

(define integer-sorts
  (~> (empty-sort-graph)
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

(define (number-term.sort x)
  (cond
    [(inexact? x) (error "not yet implemented")]
    [(zero? x) 'Zero]
    [(integer? x) (if (positive? x) 'NonZeroNatural 'NonZeroInteger)]
    [else (if (positive? x) 'PositiveRational 'NonZeroRational)]))
