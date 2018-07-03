#lang racket

(provide
 (struct-out rule)
 (struct-out equation)
 (struct-out transformation)
 (contract-out
  [make-rule (signature? term? (or/c #f term?) (or/c term? procedure?)
                         (or/c #f symbol?) boolean?
              . -> . rule?)]
  [valid-rule? (signature? any/c . -> . boolean?)]
  [rulelist? (any/c . -> . boolean?)]
  [empty-rulelist rulelist?]
  [in-rules (rulelist? . -> . stream?)]
  [add-rule (rulelist? rule? . -> . rulelist?)]
  [lookup-rules (rulelist? term? . -> . list?)]
  [merge-rulelists (rulelist? rulelist? signature? . -> . rulelist?)]
  [display-rule (rule? output-port? . -> . void?)]
  [rule-sorts-str (signature? rule? . -> . string?)]
  [make-equation (signature? term? (or/c #f term?) term? . -> . equation?)]
  [valid-equation? (signature? any/c . -> . boolean?)]
  [in-signature (rule? signature? . -> . rule?)]
  [display-equation (equation? output-port? . -> . void?)]
  [equation-sorts-str (signature? equation? . -> . string?)]
  [make-transformation (signature? rule? . -> . transformation?)]
  [transformation-sorts-str (signature? transformation? . -> . string?)]))

(require "./sorts.rkt"
         "./operators.rkt"
         "./terms.rkt")

(module+ test
  (require "./term-syntax.rkt"
           "./test-examples.rkt"
           rackunit
           racket/function
           threading))

;
; Rules and equations
;

(define (term-sort-or-kind signature term)
  (define sort (term.sort term))
  (if (term.has-vars? term)
        (kind (signature-sort-graph signature) sort)
        sort))

(define (display-label label port)
  (when label
    (display "#:label " port)
    (write label port)
    (display #\space port)))

(define (display-rule* rule port)
  (display-label (rule-label rule) port)
  (display-vars (term.vars (rule-pattern rule)) port)
  (display-term (rule-pattern rule) port)
  (display #\space port)
  (if (procedure? (rule-replacement rule))
      (display-term "<procedure>" port)
      (display-term (rule-replacement rule) port))
  (when (rule-condition rule)
    (display " #:if " port)
    (display-term (rule-condition rule) port))
  (display ")" port))

(define (display-rule rule port [mode #f])
  (display "(=> " port)
  (display-rule* rule port))

(struct rule (pattern condition replacement label)
        #:transparent
        #:methods gen:custom-write
        [(define write-proc display-rule)])

(define (display-equation equation port [mode #f])
  (display "(eq " port)
  (display-vars (set-union (term.vars (equation-left equation))
                           (term.vars (equation-right equation)))
                port)
  (display-term (equation-left equation) port)
  (display #\space port)
  (display-term (equation-right equation) port)
  (when (equation-condition equation)
    (display " #:if " port)
    (display-term (equation-condition equation) port))
  (display ")" port))

(struct equation (left condition right)
        #:transparent
        #:methods gen:custom-write
        [(define write-proc display-equation)])

;
; Construct rules and equations after checking arguments
;
(define (check-term signature term)
  (unless (allowed-term? signature term)
    (error (format "Term ~s not allowed within signature" term))))

(define (check-label label)
  (when (and label
             (not (symbol? label)))
    (error (format "Rule label not a symbol: ~s" label))))

(define (is-boolean? sort-graph sort)
  ; Accept both boolean and Boolean until spelling rules have stabilized...
  (cond
    [(has-sort? sort-graph 'boolean)
     (conforms-to? sort-graph sort 'boolean)]
    [else
     (error "signature has no boolean sort")]))

(define (check-condition signature condition allowed-vars)
  (when condition
    (define sort-graph (signature-sort-graph signature))
    (define condition-vars (term.vars condition))
    (check-term signature condition)
    (unless (conforms-to? sort-graph (term.sort condition) 'boolean)
      (error (format "Condition ~s not boolean" condition)))
    (unless (and (lookup-op signature 'true empty)
                 (lookup-op signature 'false empty))
      (error "signature does not contain true and false"))
    (unless (set-empty?
             (set-subtract condition-vars allowed-vars))
      (error (format "Condition ~s contains variables that are not used elsewhere" condition)))))

(define (make-rule signature pattern condition replacement label check-equationality?)
  (define sort-graph (signature-sort-graph signature))
  (check-term signature pattern)
  (check-label label)
  (check-condition signature condition (term.vars pattern))
  (unless (procedure? replacement)
    (check-term signature replacement)
    (define pattern-vars (term.vars pattern))
    (define replacement-vars (term.vars replacement))
    (when check-equationality?
      (unless (set-empty?
               (set-subtract replacement-vars pattern-vars))
        (error (format "Term ~s contains variables that are not in the rule pattern" replacement)))
      (unless (conforms-to? sort-graph
                            (term.sort replacement)
                            (kind sort-graph (term.sort pattern)))
        (error (format "Term ~s must be of sort ~s"
                       replacement (term.sort pattern))))))
  (rule pattern condition replacement label))

(define (valid-rule? signature rule)
  (and (rule? rule)
       (valid-term? signature (rule-pattern rule))
       (or (not (rule-condition rule))
           (valid-term? signature (rule-condition rule)))
       (or (procedure? (rule-replacement rule))
           (valid-term? signature (rule-replacement rule)))
       (or (not (rule-label rule))
           (symbol? (rule-label rule)))))

(define (rule-sorts-str signature rule)
  (define pattern-sort (term-sort-or-kind signature (rule-pattern rule)))
  (define replacement-sort (term-sort-or-kind signature (rule-replacement rule)))
  (string-append
   (constraint->string (signature-sort-graph signature) pattern-sort)
   " ⇒ "
   (constraint->string (signature-sort-graph signature) replacement-sort)))

(module+ test
  (with-signature a-signature
    (check-equal? (make-rule a-signature (T IntVar) #f (T 2) #f #t)
                  (rule (T IntVar) #f (T 2) #f))
    (check-equal? (make-rule a-signature (T IntVar) (T true) (T 2) #f #t)
                  (rule (T IntVar) (T true) (T 2) #f))
    (check-true (valid-rule? a-signature (rule (T IntVar) #f (T 2) #f)))
    (check-true (valid-rule? a-signature (rule (T IntVar) (T true) (T 2) #f)))
    (check-equal? (rule-sorts-str a-signature
                                  (make-rule a-signature (T IntVar) #f (T 2) #f #t))
                  "[ℚ] ⇒ ℕnz")
    ; Term 'bar not allowed in signature
    (check-exn exn:fail? (thunk (make-rule a-signature 'bar #f (T 2) #f #t)))
    (check-exn exn:fail? (thunk (make-rule a-signature (T Avar) 'bar (T 2) #f #t)))
    (check-exn exn:fail? (thunk (make-rule a-signature (T IntVar) #f 'bar #f #t)))
    ; Condition not a boolean
    (check-exn exn:fail? (thunk (make-rule a-signature (T IntVar) (T 0) (T 2) #f #t)))
    ; Variable in condition but not in pattern
    (check-exn exn:fail?
               (thunk (make-rule a-signature (T IntVar) (T BoolVar) (T 2) #f #t)))
    ; Variable in replacement but not in pattern
    (check-exn exn:fail?
               (thunk (make-rule a-signature (T IntVar) #f (T BoolVar) #f #t)))
    ; Replacement doesn't match sort of pattern
    (check-exn exn:fail?
               (thunk (make-rule a-signature (T IntVar) #f (T (foo a-B)) #f #t)))))

(define (make-equation signature left condition right)
  (define sort-graph (signature-sort-graph signature))
  (check-term signature left)
  (check-term signature right)
  (check-condition signature condition
                   (set-union (term.vars left) (term.vars right)))
  (define left-sort-or-kind (term-sort-or-kind signature left))
  (define right-sort-or-kind (term-sort-or-kind signature right))
  (unless (or (conforms-to? sort-graph left-sort-or-kind right-sort-or-kind)
              (conforms-to? sort-graph right-sort-or-kind left-sort-or-kind))
    (error (format "Left and right terms have incompatible sorts:\n  ~a\n  ~a"
                   left right)))
  (equation left condition right))

(define (valid-equation? signature equation)
  (and (equation? equation)
       (valid-term? signature (equation-left equation))
       (valid-term? signature (equation-right equation))
       (or (not (equation-condition equation))
           (valid-term? signature (equation-condition equation)))))

(define (equation-sorts-str signature equation)
  (define left-sort (term-sort-or-kind signature (equation-left equation)))
  (define right-sort (term-sort-or-kind signature (equation-right equation)))
  (string-append
   (constraint->string (signature-sort-graph signature) left-sort)
   " = "
   (constraint->string (signature-sort-graph signature) right-sort)))

(module+ test
  (with-signature a-signature
    (check-equal? (make-equation a-signature (T IntVar) #f (T 2))
                  (equation (T IntVar) #f (T 2)))
    (check-equal? (make-equation a-signature (T IntVar) (T true) (T 2))
                  (equation (T IntVar) (T true) (T 2)))
    (check-true (valid-equation? a-signature (equation (T IntVar) #f (T 2))))
    (check-true (valid-equation? a-signature (equation (T IntVar) (T true) (T 2))))
    (check-equal? (equation-sorts-str a-signature
                                      (make-equation a-signature (T IntVar) #f (T 2)))
                  "[ℚ] = ℕnz")
    ; Term 'bar not allowed in signature
    (check-exn exn:fail? (thunk (make-equation a-signature 'bar #f (T 2))))
    (check-exn exn:fail? (thunk (make-equation a-signature (T Avar) 'bar (T 2))))
    (check-exn exn:fail? (thunk (make-equation a-signature (T IntVar) #f 'bar)))
    ; Condition not a boolean
    (check-exn exn:fail? (thunk (make-equation a-signature (T IntVar) (T 0) (T 2))))
    ; Variable in condition but not in either term
    (check-exn exn:fail?
               (thunk (make-equation a-signature (T IntVar) (T BoolVar) (T 2))))
    ; Variable in replacement but not in either term
    (check-exn exn:fail?
               (thunk (make-equation a-signature (T IntVar) #f (T BoolVar))))
    ; Term sorts do not match
    (check-exn exn:fail?
               (thunk (make-equation a-signature (T IntVar) #f (T (foo a-B)))))))

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
                   (term.in-signature r signature)))
             (rule-label rule)
             #t))

(module+ test
  (define larger-signature
    (~> a-signature
        (add-op 'foo (list 'A) 'A)))
  (with-signature a-signature
    (check-true
     (valid-rule? larger-signature
                  (in-signature (make-rule a-signature (T foo) #f (T foo) #f #t)
                                larger-signature)))
    (check-true
     (valid-rule? larger-signature
                  (in-signature (make-rule a-signature (T IntVar) #f (T 2) #f #t)
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

(define (merge-rulelists rl1 rl2 merged-signature)
  (for/fold ([rules empty-rulelist])
            ([rule (stream-append (in-rules rl1) (in-rules rl2))])
    (add-rule rules (in-signature rule merged-signature))))

(module+ test
  (with-signature a-signature
    (define rule1 (make-rule a-signature (T IntVar) #f (T 2) #f #t))
    (define rule2 (make-rule a-signature (T (foo Bvar)) #f (T Bvar) #f #t))
    (define rule3 (make-rule a-signature (T (foo Avar Bvar)) #f (T (foo Bvar)) #f #t))
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
    (check-equal? (merge-rulelists empty-rulelist some-rules a-signature)
                  some-rules)
    (check-equal? (merge-rulelists some-rules empty-rulelist a-signature)
                  some-rules)))

;
; Transformations are rewrite rules that are applied explicitly to
; arbitrary terms or equations, rather than being used in the process of
; context-based reduction.
;
; Since transformations can be applied to patterns containing variables,
; their own variables are replaced to gensym-based variables to avoid
; accidental name clashes.
;
(define (display-transformation tr port [mode #f])
  (display "(tr " port)
  (display-rule* (transformation-rule tr) port))

(struct transformation (rule converted-rule)
        #:transparent
        #:methods gen:custom-write
        [(define write-proc display-transformation)]
        #:methods gen:equal+hash
        [(define (equal-proc t1 t2 _)
           (equal? (transformation-rule t1) (transformation-rule t2)))
         (define (hash-proc t _)
           (equal-hash-code (transformation-rule t)))
         (define (hash2-proc t _)
           (equal-secondary-hash-code (transformation-rule t)))])

(define (make-transformation signature rule)
  (define sorts (signature-sort-graph signature))
  (define pattern (rule-pattern rule))
  (define condition (rule-condition rule))
  (define replacement (rule-replacement rule))
  (define var-substitution
    (for/fold ([s empty-substitution])
              ([var (term.vars pattern)])
      (merge-substitutions
       s
       (one-match signature var
                  (make-unique-var sorts (var-name var) (var-sort var))))))
  (transformation
   rule
   (make-rule signature
              (term.substitute signature pattern var-substitution)
              (if condition
                  (term.substitute signature condition var-substitution)
                  #f)
              (term.substitute signature replacement var-substitution)
              #f
              #f)))

(define (transformation-sorts-str signature transformation)
  (define rule (transformation-rule transformation))
  (define pattern-sort (term-sort-or-kind signature (rule-pattern rule)))
  (define replacement-sort (term-sort-or-kind signature (rule-replacement rule)))
  (string-append
   (constraint->string (signature-sort-graph signature) pattern-sort)
   " → "
   (constraint->string (signature-sort-graph signature) replacement-sort)))
