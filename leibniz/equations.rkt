#lang racket

(provide
 (struct-out rule)
 (contract-out
  [make-rule (signature? term? (or/c #f term?) (or/c term? procedure?)
              . -> . rule?)]
  [valid-rule? (signature? any/c . -> . boolean?)]
  [rulelist? (any/c . -> . boolean?)]
  [empty-rulelist rulelist?]
  [in-rules (rulelist? . -> . stream?)]
  [add-rule (rulelist? rule? . -> . rulelist?)]
  [lookup-rules (rulelist? term? . -> . list?)]
  [merge-rulelists (signature? rulelist? rulelist? . -> . rulelist?)]))

(require "./sorts.rkt"
         "./operators.rkt"
         "./terms.rkt")

(module+ test
  (require "./term-syntax.rkt"
           "./test-examples.rkt"
           rackunit
           racket/function
           rackjure/threading))

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
  (unless (or (procedure? replacement)
              (allowed-term? signature replacement))
    (error (format "Term ~s not allowed within signature" replacement)))
  (define pattern-vars (term.vars pattern))
  (define condition-vars (and condition (term.vars condition)))
  (define replacement-vars (if (procedure? replacement)
                               (set)
                               (term.vars replacement)))
  (define sort-graph (signature-sort-graph signature))
  (when condition
    (unless (conforms-to? sort-graph (term.sort condition) 'Boolean)
      (error (format "Condition ~s not of sort Boolean" condition)))
    (unless (and (lookup-op signature 'true empty)
                 (lookup-op signature 'false empty))
      (error "signature does not contain true and false"))
    (unless (set-empty?
             (set-subtract condition-vars pattern-vars))
      (error (format "Condition ~s contains variables that are not in the rule pattern" condition))))
  (unless (set-empty?
           (set-subtract replacement-vars pattern-vars))
    (error (format "Term ~s contains variables that are not in the rule pattern" replacement)))
  (unless (or (procedure? replacement)
              (conforms-to? sort-graph
                            (term.sort replacement) (term.sort pattern)))
    (error (format "Term ~s must be of sort ~s"
                   replacement (term.sort pattern))))
  (rule pattern condition replacement))

(define (valid-rule? signature rule)
  (and (rule? rule)
       (valid-term? signature (rule-pattern rule))
       (or (not (rule-condition rule))
           (valid-term? signature (rule-condition rule)))
       (or (procedure? (rule-replacement rule))
           (valid-term? signature (rule-replacement rule)))))

(module+ test
  (with-sig-and-vars a-signature a-varset
    (check-equal? (make-rule a-signature (T IntVar) #f (T 2))
                  (rule (T IntVar) #f (T 2)))
    (check-equal? (make-rule a-signature (T IntVar) (T true) (T 2))
                  (rule (T IntVar) (T true) (T 2)))
    (check-true (valid-rule? a-signature (rule (T IntVar) #f (T 2))))
    (check-true (valid-rule? a-signature (rule (T IntVar) (T true) (T 2))))
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

; Convert a rule to a new (larger) signature. Used for merging contexts.
(define (in-signature rule signature)
  (make-rule signature
             (term.in-signature (rule-pattern rule) signature)
             (let ([c (rule-condition rule)])
               (if c
                   (term.in-signature c signature)
                   c))
             (let ([r (rule-replacement rule)])
               (if (procedure? r)
                   r
                   (term.in-signature r signature))) ))

(module+ test
  (define larger-signature
    (~> a-signature
        (add-op 'foo (list 'A) 'A)))
  (with-sig-and-vars a-signature a-varset
    (check-true
     (valid-rule? larger-signature
                  (in-signature (make-rule a-signature (T foo) #f (T foo))
                                larger-signature)))
    (check-true
     (valid-rule? larger-signature
                  (in-signature (make-rule a-signature (T IntVar) #f (T 2))
                                larger-signature)))))

;
; Rule lists
; For efficiency, a rule list is organized as a hash of sublists, indexed
; by the key of its pattern.
;
(define (rulelist? x)
  (hash? x))

(define empty-rulelist
  (hash))

(define (add-rule rulelist rule)
  (let* ([pattern (rule-pattern rule)]
         [key (term.key pattern)])
    (hash-update rulelist
                 key
                 (λ (l) (append l (list rule)))
                 empty)))

(define (lookup-rules rulelist term)
  (hash-ref rulelist (term.key term) empty))

(define (in-rules rulelist)
  (for*/stream ([(key rules) rulelist]
                [rule rules])
     rule))

(define (merge-rulelists merged-signature rl1 rl2)
  (for/fold ([rules empty-rulelist])
            ([rule (stream-append (in-rules rl1) (in-rules rl2))])
    (add-rule rules (in-signature rule merged-signature))))

(module+ test
  (with-sig-and-vars a-signature a-varset
    (define rule1 (make-rule a-signature (T IntVar) #f (T 2)))
    (define rule2 (make-rule a-signature (T (foo Bvar)) #f (T Bvar)))
    (define rule3 (make-rule a-signature (T (foo Avar Bvar)) #f (T (foo Bvar))))
    (define some-rules
      (~> empty-rulelist
          (add-rule rule1)
          (add-rule rule2)
          (add-rule rule3)))
    (check-equal? (hash-count some-rules) 2)
    (check-equal? (length (lookup-rules some-rules (T foo))) 2)
    (check-equal? (length (lookup-rules some-rules (T IntVar))) 1)
    (check-true (empty? (lookup-rules some-rules (T an-A))))
    (check-equal? (stream-length (in-rules some-rules)) 3)
    (check-equal? (stream->list (in-rules some-rules)) (list rule1 rule2 rule3))
    (check-equal? (merge-rulelists a-signature empty-rulelist some-rules) some-rules)
    (check-equal? (merge-rulelists a-signature some-rules empty-rulelist) some-rules)))
