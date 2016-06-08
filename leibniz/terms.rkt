#lang racket

(require "./lightweight-class.rkt"
         "./sorts.rkt"
         "./operators.rkt"
         "./numbers.rkt"
         racket/generic
         rackjure/threading
         racket/generator)

(module+ test
  (require rackunit racket/function rackjure/threading)
  ; Define a simple sort graph and signaturefor testing
  (define sorts
    (~> exact-number-sorts
        (add-sort 'A) (add-sort 'B)
        (add-subsort-relation 'B 'A)
        (add-sort 'X) (add-sort 'Y)
        (add-subsort-relation 'Y 'X)))
  (define a-signature
    (~> (empty-signature sorts)
        (add-op 'an-A empty 'A)
        (add-op 'a-B empty 'B)
        (add-op 'an-X empty 'X)
        (add-op 'a-Y empty 'Y)
        (add-op 'foo empty 'B)
        (add-op 'foo (list 'B) 'A)
        (add-op 'foo (list 'A 'B) 'A))))

;
; The generic interface for terms
;
(define-generics term
  [term.sort term]
  #:fast-defaults
  ([number?
    (define term.sort number-term.sort)]))

(module+ test
  (check-equal? (term.sort 0) 'Zero)
  (check-equal? (term.sort 1) 'NonZeroNatural)
  (check-equal? (term.sort -1) 'NonZeroInteger)
  (check-equal? (term.sort 1/2) 'PositiveRational)
  (check-equal? (term.sort -1/2) 'NonZeroRational))

;
; Operator-based terms
;
(struct op-term (op args sort)
  #:transparent
  #:methods gen:term
  [(define (term.sort t)
     (op-term-sort t))])

(define (make-term signature op args)
  (define rank (lookup-op signature op (map term.sort args)))
  (and rank
      (op-term op args (cdr rank))))

(module+ test
  (define an-A (make-term a-signature 'an-A empty))
  (check-equal? (term.sort an-A) 'A)
  (define a-B (make-term a-signature 'a-B empty))
  (check-equal? (term.sort a-B) 'B)
  (define an-X (make-term a-signature 'an-X empty))
  (check-equal? (term.sort an-X) 'X)
  (define a-Y (make-term a-signature 'a-Y empty))
  (check-equal? (term.sort a-Y) 'Y)
 
  (check-equal? (term.sort (make-term a-signature 'foo empty)) 'B)
  (check-equal? (term.sort (make-term a-signature 'foo (list a-B))) 'A)
  (check-equal? (term.sort (make-term a-signature 'foo (list an-A a-B))) 'A)
  (check-equal? (term.sort (make-term a-signature 'foo (list a-B a-B))) 'A)
  (check-equal? (term.sort (make-term a-signature 'foo (list an-A)))
                (kind sorts 'A))
  (check-equal? (term.sort (make-term a-signature 'foo (list an-A an-A)))
                (kind sorts 'A))
  (check-false (make-term a-signature 'foo (list an-X)))
)

