#lang racket

(provide
 (struct-out rule)
 (contract-out
  [make-rule ((signature? term? (or/c #f term?) (or/c term? procedure?))
              ((or/c #f symbol?))
              . ->* . rule?)]
  [valid-rule? (signature? any/c . -> . boolean?)]
  [rulelist? (any/c . -> . boolean?)]
  [empty-rulelist rulelist?]
  [in-rules (rulelist? . -> . stream?)]
  [add-rule (rulelist? rule? . -> . rulelist?)]
  [lookup-rules (rulelist? term? . -> . list?)]
  [merge-rulelists (signature? rulelist? rulelist? . -> . rulelist?)]
  [write-rule (rule? output-port? . -> . void?)]
  [make-equation ((signature? term? (or/c #f term?) term?) ((or/c #f symbol?))
                  . ->* . equation?)]
  [valid-equation? (signature? any/c . -> . boolean?)]
  [equationset? (any/c . -> . boolean?)]
  [empty-equationset equationset?]
  [in-equations (equationset? . -> . stream?)]
  [add-equation (equationset? equation? . -> . equationset?)]
  [merge-equationsets (signature? equationset? equationset? . -> . equationset?)]
  [write-equation (equation? output-port? . -> . void?)]))

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
; Rules and equations
;

(define (write-label label port)
  (when label
    (write-string "#:label " port)
    (write label port)
    (write-char #\space port)))

(define (write-vars vars port)
  (unless (set-empty? vars)
    (write-string
     (string-join (for/list ([var (in-set vars)])
                    (format "[~s ~s]" (var-name var) (var-sort var)))
                  " "
                  #:before-first "#:vars ("
                  #:after-last ")  ")
     port)))

(define (write-rule rule port [mode #f])
  (write-string "(=> " port)
  (write-label (rule-label rule) port)
  (write-vars (term.vars (rule-pattern rule)) port)
  (write (rule-pattern rule) port)
  (write-char #\space port)
  (write (rule-replacement rule) port)
  (when (rule-condition rule)
    (write-string " #:if " port)
    (write (rule-condition rule) port))
  (write-string ")" port))

(struct rule (pattern condition replacement label)
        #:transparent
        #:methods gen:custom-write
        [(define write-proc write-rule)])

(define (write-equation equation port [mode #f])
  (write-string "(eq " port)
  (write-label (equation-label equation) port)
  (write-vars (set-union (term.vars (equation-left equation))
                         (term.vars (equation-right equation)))
              port)
  (write (equation-left equation) port)
  (write-char #\space port)
  (write (equation-right equation) port)
  (when (equation-condition equation)
    (write-string " #:if " port)
    (write (equation-condition equation) port))
  (write-string ")" port))

(struct equation (left condition right label)
        #:transparent
        #:methods gen:custom-write
        [(define write-proc write-equation)])

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

(define (check-condition signature condition allowed-vars)
  (when condition
    (define sort-graph (signature-sort-graph signature))
    (define condition-vars (term.vars condition))
    (check-term signature condition)
    (unless (conforms-to? sort-graph (term.sort condition) 'Boolean)
      (error (format "Condition ~s not of sort Boolean" condition)))
    (unless (and (lookup-op signature 'true empty)
                 (lookup-op signature 'false empty))
      (error "signature does not contain true and false"))
    (unless (set-empty?
             (set-subtract condition-vars allowed-vars))
      (error (format "Condition ~s contains variables that are not used elsewhere" condition)))))

(define (make-rule signature pattern condition replacement [label #f])
  (define sort-graph (signature-sort-graph signature))
  (check-term signature pattern)
  (check-label label)
  (check-condition signature condition (term.vars pattern))
  (unless (procedure? replacement)
    (check-term signature replacement)
    (define pattern-vars (term.vars pattern))
    (define replacement-vars (term.vars replacement))
    (unless (set-empty?
             (set-subtract replacement-vars pattern-vars))
      (error (format "Term ~s contains variables that are not in the rule pattern" replacement)))
    (unless (conforms-to? sort-graph
                          (term.sort replacement) (term.sort pattern))
      (error (format "Term ~s must be of sort ~s"
                     replacement (term.sort pattern)))))
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

(module+ test
  (with-sig-and-vars a-signature a-varset
    (check-equal? (make-rule a-signature (T IntVar) #f (T 2))
                  (rule (T IntVar) #f (T 2) #f))
    (check-equal? (make-rule a-signature (T IntVar) (T true) (T 2))
                  (rule (T IntVar) (T true) (T 2) #f))
    (check-true (valid-rule? a-signature (rule (T IntVar) #f (T 2) #f)))
    (check-true (valid-rule? a-signature (rule (T IntVar) (T true) (T 2) #f)))
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

(define (make-equation signature left condition right [label #f])
  (define sort-graph (signature-sort-graph signature))
  (check-term signature left)
  (check-term signature right)
  (check-label label)
  (check-condition signature condition
                   (set-union (term.vars left) (term.vars right)))
  (define left-sort-or-kind
    (if (term.has-vars? left)
        (kind sort-graph (term.sort left))
        (term.sort left)))
  (define right-sort-or-kind
    (if (term.has-vars? right)
        (kind sort-graph (term.sort right))
        (term.sort right)))
  (unless (or (conforms-to? sort-graph left-sort-or-kind right-sort-or-kind)
              (conforms-to? sort-graph right-sort-or-kind left-sort-or-kind))
    (error "Left and right terms have incompatible sorts"))
  (equation left condition right label))

(define (valid-equation? signature equation)
  (and (equation? equation)
       (valid-term? signature (equation-left equation))
       (valid-term? signature (equation-right equation))
       (or (not (equation-condition equation))
           (valid-term? signature (equation-condition equation)))
       (or (not (equation-label equation))
           (symbol? (equation-label equation)))))

(module+ test
  (with-sig-and-vars a-signature a-varset
    (check-equal? (make-equation a-signature (T IntVar) #f (T 2))
                  (equation (T IntVar) #f (T 2) #f))
    (check-equal? (make-equation a-signature (T IntVar) (T true) (T 2))
                  (equation (T IntVar) (T true) (T 2) #f))
    (check-true (valid-equation? a-signature (equation (T IntVar) #f (T 2) #f)))
    (check-true (valid-equation? a-signature (equation (T IntVar) (T true) (T 2) #f)))
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
             (rule-label rule)))

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
    (check-equal? (list->set (stream->list (in-rules some-rules)))
                  (set rule1 rule2 rule3))
    (check-equal? (merge-rulelists a-signature empty-rulelist some-rules)
                  some-rules)
    (check-equal? (merge-rulelists a-signature some-rules empty-rulelist)
                  some-rules)))

;
; Equation sets
; For now, plain sets. Once it is clear how equations will be used,
; this choice may be revised.
;
(define (equationset? x)
  (set? x))

(define empty-equationset
  (set))

(define (add-equation equationset equation)
  (set-add equationset equation))

(define (in-equations equationset)
  (set->stream equationset))

(define (merge-equationsets merged-signature el1 el2)
  (for/set ([equation (stream-append (in-equations el1) (in-equations el2))])
    (make-equation merged-signature
                   (term.in-signature (equation-left equation) merged-signature)
                   (let ([c (equation-condition equation)])
                     (if c
                         (term.in-signature c merged-signature)
                         c))
                   (term.in-signature (equation-right equation) merged-signature)
                   (equation-label equation))))

(module+ test
  (with-sig-and-vars a-signature a-varset
    (define equation1 (make-equation a-signature (T IntVar) #f (T 2)))
    (define equation2 (make-equation a-signature (T (foo Bvar)) #f (T Bvar)))
    (define equation3 (make-equation a-signature (T (foo Avar Bvar)) #f (T (foo Bvar))))
    (define some-equations
      (~> empty-equationset
          (add-equation equation1)
          (add-equation equation2)
          (add-equation equation1)
          (add-equation equation3)))
    (check-equal? (set-count some-equations) 3)
    (check-equal? (stream-length (in-equations some-equations)) 3)
    (check-equal? (list->set (stream->list (in-equations some-equations)))
                  some-equations)
    (check-equal? (merge-equationsets a-signature empty-equationset some-equations)
                  some-equations)
    (check-equal? (merge-equationsets a-signature some-equations empty-equationset)
                  some-equations)))
