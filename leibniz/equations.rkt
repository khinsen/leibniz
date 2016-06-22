#lang racket

(require "./sorts.rkt"
         "./operators.rkt"
         "./terms.rkt"
)

(module+ test
  (require "./term-syntax.rkt"
           "./test-examples.rkt"
           rackunit
           racket/function))

;
; Rules
;
(struct rule (pattern condition replacement)
        #:transparent)

(define (make-rule signature pattern condition replacement)
  (unless (allowed-term? signature pattern)
    (error (format "Pattern ~s not allowed within signature" pattern)))
  (when (and condition (not (allowed-term? signature condition)))
    (error (format "Term ~s not allowed within signature" condition)))
  (unless (allowed-term? signature replacement)
    (error (format "Term ~s not allowed within signature" replacement)))
  (define pattern-vars (term.vars pattern))
  (define condition-vars (and condition (term.vars condition)))
  (define replacement-vars (term.vars replacement))
  (define sort-graph (signature-sort-graph signature))
  (when condition
    (unless (conforms-to? sort-graph (term.sort condition) 'Boolean)
      (error (format "Condition ~s not of sort Boolean" condition)))
    (unless (set-empty?
             (set-subtract condition-vars pattern-vars))
      (error (format "Condition ~s contains variables that are not in the rule pattern" condition))))
  (unless (set-empty?
           (set-subtract replacement-vars pattern-vars))
    (error (format "Term ~s contains variables that are not in the rule pattern" replacement)))
  (unless (conforms-to? sort-graph
                        (term.sort replacement) (term.sort pattern))
    (error (format "Term ~s must be of sort ~s"
                   replacement (term.sort pattern))))
  (rule pattern condition replacement))

(module+ test
  (with-sig-and-vars a-signature a-varset
    (check-equal? (make-rule a-signature (T IntVar) #f (T 2))
                  (rule (T IntVar) #f (T 2)))
    (check-equal? (make-rule a-signature (T IntVar) (T true) (T 2))
                  (rule (T IntVar) (T true) (T 2)))
    ; Term 'bar not allowed in signature
    (check-exn exn:fail? (thunk (make-rule a-signature 'bar #f (T 2))))
    (check-exn exn:fail? (thunk (make-rule a-signature (T Avar) 'bar (T 2))))
    (check-exn exn:fail? (thunk (make-rule a-signature (T IntVar) #f 'bar)))
    ; Condition not a boolean
    (check-exn exn:fail? (thunk (make-rule a-signature (T IntVar) (T 0) (T 2))))
    ; Variable in condition but not in pattern
    (check-exn exn:fail?
               (thunk (make-rule a-signature (T IntVar) (T BoolVar) (T 2))))
    ; Variable in replacement but not in pattern
    (check-exn exn:fail?
               (thunk (make-rule a-signature (T IntVar) #f (T BoolVar))))
    ; Replacement doesn't match sort of pattern
    (check-exn exn:fail?
               (thunk (make-rule a-signature (T IntVar) #f (T (foo a-B)))))))
