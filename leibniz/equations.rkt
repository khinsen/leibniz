#lang racket

(provide
 (struct-out p-rule)
 (contract-out
  [valid-rule? (signature? any/c . -> . boolean?)]
  [make-p-rule (signature? term? (or/c #f term?) (or/c term? procedure?)
               . -> . p-rule?)]
  [make-fn-rule (signature? symbol? (and/c integer? positive?) procedure?
                . -> . fn-rule?)]
  [rule-match (signature? rule? term? . -> . sequence?)]
  [rulelist? (any/c . -> . boolean?)]
  [empty-rulelist rulelist?]
  [in-rules (rulelist? . -> . stream?)]
  [add-rule (rulelist? rule? . -> . rulelist?)]
  [lookup-rules (rulelist? term? . -> . list?)]
  [merge-rulelists (signature? rulelist? rulelist? . -> . rulelist?)]))

(require "./sorts.rkt"
         "./operators.rkt"
         "./terms.rkt"
         racket/generator)

(module+ test
  (require "./term-syntax.rkt"
           "./test-examples.rkt"
           rackunit
           racket/function
           rackjure/threading))

;
; Rules are either pattern rules or function rules
;
(define (rule? x)
  (or (p-rule? x)
      (fn-rule? x)))

(define (valid-rule? signature x)
  (or (valid-p-rule? signature x)
      (valid-fn-rule? signature x)))

;
; Pattern rules
;
(struct p-rule (pattern condition replacement)
        #:transparent)

(define (make-p-rule signature pattern condition replacement)
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
  (p-rule pattern condition replacement))

(define (valid-p-rule? signature rule)
  (and (p-rule? rule)
       (valid-term? signature (p-rule-pattern rule))
       (or (not (p-rule-condition rule))
           (valid-term? signature (p-rule-condition rule)))
       (or (procedure? (p-rule-replacement rule))
           (valid-term? signature (p-rule-replacement rule)))))

(module+ test
  (with-sig-and-vars a-signature a-varset
    (check-equal? (make-p-rule a-signature (T IntVar) #f (T 2))
                  (p-rule (T IntVar) #f (T 2)))
    (check-equal? (make-p-rule a-signature (T IntVar) (T true) (T 2))
                  (p-rule (T IntVar) (T true) (T 2)))
    (check-true (valid-p-rule? a-signature (p-rule (T IntVar) #f (T 2))))
    (check-true (valid-p-rule? a-signature (p-rule (T IntVar) (T true) (T 2))))
    ; Term 'bar not allowed in signature
    (check-exn exn:fail? (thunk (make-p-rule a-signature 'bar #f (T 2))))
    (check-exn exn:fail? (thunk (make-p-rule a-signature (T Avar) 'bar (T 2))))
    (check-exn exn:fail? (thunk (make-p-rule a-signature (T IntVar) #f 'bar)))
    ; Condition not a boolean
    (check-exn exn:fail? (thunk (make-p-rule a-signature (T IntVar) (T 0) (T 2))))
    ; Variable in condition but not in pattern
    (check-exn exn:fail?
               (thunk (make-p-rule a-signature (T IntVar) (T BoolVar) (T 2))))
    ; Variable in replacement but not in pattern
    (check-exn exn:fail?
               (thunk (make-p-rule a-signature (T IntVar) #f (T BoolVar))))
    ; Replacement doesn't match sort of pattern
    (check-exn exn:fail?
               (thunk (make-p-rule a-signature (T IntVar) #f (T (foo a-B)))))))

;
; Function rules
;
(struct fn-rule (op n-args fn)
        #:transparent)

(define (make-fn-rule signature op n-args fn)
  (unless (has-unrestricted-arity? signature op n-args)
    (error (format "Operator ~a has no unrestricted arity for ~a arguments" op n-args)))
  (fn-rule op n-args fn))

(define (valid-fn-rule? signature rule)
  (fn-rule? rule))

;
; Match a rule to a term
;
(define (rule-match signature rule term)

  (define (p-rule-match signature rule term)
    (define pattern (p-rule-pattern rule))
    (define condition (p-rule-condition rule))
    (define replacement (p-rule-replacement rule))
    (in-generator #:arity 2
      (for ([substitution (term.match signature pattern term)])
        (yield (if condition
                   (thunk (term.substitute signature condition substitution))
                   #f)
               (if (procedure? replacement)
                   (thunk (with-handlers ([exn:fail? (lambda (v) #f)])
                            (replacement signature
                                         pattern condition substitution)))
                   (thunk (term.substitute signature replacement substitution)))))))

  (define (fn-rule-match signature rule term)
    (define-values (op args) (term.op-and-args term))
    (if (and (equal? op (fn-rule-op rule))
             (equal? (length args) (fn-rule-n-args rule)))
        (in-generator #:arity 2
          (yield #f
                 (thunk (with-handlers ([exn:fail? (lambda (v) #f)])
                          ((fn-rule-fn rule) signature op args)))))
        empty-sequence))

  (if (p-rule? rule)
      (p-rule-match signature rule term)
      (fn-rule-match signature rule term)))

;
; Convert a rule to a new (larger) signature. Used for merging contexts.
;
(define (in-signature rule signature)
  (if (p-rule? rule)
      (make-p-rule signature
                   (term.in-signature (p-rule-pattern rule) signature)
                   (let ([c (p-rule-condition rule)])
                     (if c
                         (term.in-signature c signature)
                         c))
                   (let ([r (p-rule-replacement rule)])
                     (if (procedure? r)
                         r
                         (term.in-signature r signature))) )
      (make-fn-rule signature
                    (fn-rule-op rule)
                    (fn-rule-n-args rule)
                    (fn-rule-fn rule))))

(module+ test
  (define larger-signature
    (~> a-signature
        (add-op 'foo (list 'A) 'A)))
  (with-sig-and-vars a-signature a-varset
    (check-true
     (valid-rule? larger-signature
                  (in-signature (make-p-rule a-signature (T foo) #f (T foo))
                                larger-signature)))
    (check-true
     (valid-rule? larger-signature
                  (in-signature (make-p-rule a-signature (T IntVar) #f (T 2))
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
  (define key (if (p-rule? rule)
                  (term.key (p-rule-pattern rule))
                  (fn-rule-op rule)))
  (hash-update rulelist
                 key
                 (λ (l) (append l (list rule)))
                 empty))

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
    (define rule1 (make-p-rule a-signature (T IntVar) #f (T 2)))
    (define rule2 (make-p-rule a-signature (T (foo Bvar)) #f (T Bvar)))
    (define rule3 (make-p-rule a-signature (T (foo Avar Bvar)) #f (T (foo Bvar))))
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
    (check-equal? (list->set (stream->list (in-rules some-rules)))
                  (set rule1 rule2 rule3))
    (check-equal? (merge-rulelists a-signature empty-rulelist some-rules)
                  some-rules)
    (check-equal? (merge-rulelists a-signature some-rules empty-rulelist)
                  some-rules)))
