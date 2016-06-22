#lang racket

(provide
 (contract-out
  [term?         (any/c . -> . boolean?)]
  [term.sort     (term? . -> . sort-or-kind?)]
  [term.vars     (term? . -> . set?)]
  [term.key      (term? . -> . symbol?)]
  [allowed-term? (signature? term? . -> . boolean?)]
  [make-term     (signature? symbol? list? . -> . (or/c #f term?))]
  [empty-varset  (sort-graph? . -> . varset?)]
  [add-var       (varset? symbol? sort-or-kind? . -> . varset?)]
  [var?          (any/c . -> . boolean?)]
  [make-var      (varset? symbol? . -> . (or/c #f var?))]))

(require "./lightweight-class.rkt"
         "./sorts.rkt"
         "./operators.rkt"
         "./builtins.rkt"
         "./condd.rkt"
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
(define (non-pattern-match signature term1 term2)
  (conditional-match (equal? term1 term2) empty-substitution))

(define (non-pattern-substitute signature term substitution)
  term)

(define (non-pattern-vars term)
  (set))

(define-generics term
  [term.sort term]
  [term.builtin-type term]
  [term.key term]
  [term.has-vars? term]
  [term.vars term]
  [term.match signature term other]
  [term.substitute signature term substitution]
  #:fast-defaults
  ([number?
    (define term.sort number-term.sort)
    (define term.builtin-type number-term.builtin-type)
    (define term.key term.builtin-type)
    (define (term.has-vars? x) #f)
    (define term.vars non-pattern-vars)
    (define term.match non-pattern-match)
    (define term.substitute non-pattern-substitute)]
   [symbol?
    (define (term.sort x) 'Symbol)
    (define (term.builtin-type x) '*symbol*)
    (define term.key term.builtin-type)
    (define (term.has-vars? x) #f)
    (define term.vars non-pattern-vars)
    (define term.match non-pattern-match)
    (define term.substitute non-pattern-substitute)]
   [string?
    (define (term.sort x) 'String)
    (define (term.builtin-type x) '*string*)
    (define term.key term.builtin-type)
    (define (term.has-vars? x) #f)
    (define term.vars non-pattern-vars)
    (define term.match non-pattern-match)
    (define term.substitute non-pattern-substitute)])
  #:fallbacks
  [(define (term.builtin-type x) #f)
   (define (term.has-vars? x) #f)
   (define term.vars non-pattern-vars)
   (define term.match non-pattern-match)
   (define term.substitute non-pattern-substitute)])

(module+ test
  (check-equal? (term.sort 0) 'Zero)
  (check-equal? (term.sort 1) 'NonZeroNatural)
  (check-equal? (term.sort -1) 'NonZeroInteger)
  (check-equal? (term.sort 1/2) 'PositiveRational)
  (check-equal? (term.sort -1/2) 'NonZeroRational)
  (check-equal? (term.builtin-type 0) '*integer*)
  (check-equal? (term.builtin-type 1/2) '*rational*)
  (check-equal? (term.key 0) '*integer*)
  (check-equal? (term.key 1/2) '*rational*)
  (check-false (term.has-vars? 1))
  (check-equal? (term.sort 'foo) 'Symbol)
  (check-equal? (term.builtin-type 'foo) '*symbol*)
  (check-equal? (term.key 'foo) '*symbol*)
  (check-false (term.has-vars? 'foo))
  (check-equal? (term.sort "foo") 'String)
  (check-equal? (term.builtin-type "foo") '*string*)
  (check-equal? (term.key "foo") '*string*)
  (check-false (term.has-vars? "foo")))

;
; Substitutions are var->term hashes describing a match.
; Combining two substitutions that are contradictory leads to
; a non-match signalled by the special value #f.
;
(define empty-substitution (hash))

(define (substitution var term)
  (hash var term))

(define (merge-substitutions s1 s2)
  (if (not s2)
      #f
      (for/fold ([s-acc s1])
                ([(var value) s2])
        #:break (not s-acc)
        (if (and (hash-has-key? s-acc var)
                 (not (equal? (hash-ref s-acc var) value)))
            #f
            (hash-set s-acc var value)))))

(define (substitution-value substitution var)
  (hash-ref substitution var #f))

(module+ test
  ; Substitutions are meant to be used as var->term mappings,
  ; but they are really just hash maps with special key collision
  ; handling. The tests use symbol keys for simplicity.
  (check-equal? (merge-substitutions (substitution 'A 1)
                                     (substitution 'B 2))
                (hash 'A 1 'B 2))
  (check-equal? (merge-substitutions
                 (merge-substitutions (substitution 'A 1)
                                      (substitution 'B 2))
                 (substitution 'C 3))
                (hash 'A 1 'B 2 'C 3))
  (check-false (merge-substitutions (substitution 'A 1)
                                    (substitution 'A 2)))
  (check-false (merge-substitutions #f (substitution 'A 2)))
  (check-false (merge-substitutions (substitution 'A 2) #f))
  (check-equal? (substitution-value
                 (merge-substitutions (substitution 'A 1)
                                      (substitution 'B 2))
                 'B) 2)
  (check-false (substitution-value
                (merge-substitutions (substitution 'A 1)
                                     (substitution 'B 2))
                'C)))

;
; Matching returns a sequence of all match-generating substitutions.
;
(define no-match empty-sequence)

(define (single-match substitution)
  (in-value substitution))

(define-syntax-rule (conditional-match pred? substitution)
  (if pred?
      (single-match substitution)
      no-match))

(define (all-matches signature pattern term)
  (sequence->list (term.match signature pattern term)))

(module+ test
  (define-simple-check (check-no-match signature pattern term)
    (= 0 (sequence-length (term.match signature pattern term))))
  (define-simple-check (check-single-match signature pattern term substitution)
    (equal? (all-matches signature pattern term) (list substitution)))
  (define-simple-check (check-self-substitution signature pattern term)
    (equal? (term.substitute signature pattern
                             (first (all-matches signature pattern term)))
            term))
  (define-simple-check (check-no-substitution signature pattern)
    (equal? (term.substitute signature pattern
                             (substitution
                              (make-var a-varset 'StrangelyNamedVar)
                              0))
            pattern)))

(module+ test
  (check-single-match a-signature 1 1 empty-substitution)
  (check-no-match a-signature 0 1))

;
; Operator-defined terms
;
(struct op-term (signature op args sort)
  #:transparent
  #:methods gen:term
  [(define (term.sort t)
     (op-term-sort t))
   (define (term.key t)
     (op-term-op t))])

(module+ test
  (define an-A (make-term a-signature 'an-A empty))
  (check-equal? (term.sort an-A) 'A)
  (check-equal? (term.key an-A) 'an-A)
  (check-false (term.has-vars? an-A))
  (define a-B (make-term a-signature 'a-B empty))
  (check-equal? (term.sort a-B) 'B)
  (check-equal? (term.key a-B) 'a-B)
  (check-false (term.has-vars? a-B))
  (define an-X (make-term a-signature 'an-X empty))
  (check-equal? (term.sort an-X) 'X)
  (check-equal? (term.key an-X) 'an-X)
  (check-false (term.has-vars? an-X))
  (define a-Y (make-term a-signature 'a-Y empty))
  (check-equal? (term.sort a-Y) 'Y)
  (check-equal? (term.key a-Y) 'a-Y)
  (check-false (term.has-vars? a-Y))
 
  (check-equal? (term.sort (make-term a-signature 'foo empty)) 'B)
  (check-equal? (term.sort (make-term a-signature 'foo (list a-B))) 'A)
  (check-equal? (term.sort (make-term a-signature 'foo (list an-A a-B))) 'A)
  (check-equal? (term.sort (make-term a-signature 'foo (list a-B a-B))) 'A)
  (check-equal? (term.sort (make-term a-signature 'foo (list an-A)))
                (kind sorts 'A))
  (check-equal? (term.sort (make-term a-signature 'foo (list an-A an-A)))
                (kind sorts 'A))
  (check-equal? (term.key (make-term a-signature 'foo empty)) 'foo)
  (check-false (make-term a-signature 'foo (list an-X)))

  (check-single-match a-signature
                      (make-term a-signature 'foo (list a-B))
                      (make-term a-signature 'foo (list a-B))
                      empty-substitution)
  (check-no-match a-signature
                  (make-term a-signature 'foo (list a-B))
                  (make-term a-signature 'foo empty)))

;
; Varsets
;
(define-class varset 

  (field sort-graph vars)

  (define (add-var symbol sort-or-kind)
        (when (hash-has-key? vars symbol)
          (error (format "symbol ~a already used")))
        (validate-sort-constraint sort-graph sort-or-kind)
        (varset sort-graph
                (hash-set vars symbol sort-or-kind)))

  (define (lookup-var symbol)
    (hash-ref vars symbol #f)))

(define (empty-varset sort-graph)
  (varset sort-graph (hash)))

(module+ test
  (define some-varset
    (~> (empty-varset sorts)
        (add-var 'X 'A)))
  (check-equal? (lookup-var some-varset 'X) 'A)
  (check-false (lookup-var some-varset 'foo))
  (check-exn exn:fail? (thunk (add-var some-varset 'X 'X)))
  (check-exn exn:fail? (thunk (add-var some-varset 'Z 'Z))))

;
; Variables
;
(struct var (sort-graph name sort-or-kind)
  #:transparent
  #:methods gen:term
  [(define/generic generic-sort term.sort)
   (define (term.sort t)
     (var-sort-or-kind t))
   (define (term.key t)
     '*variable*)
   (define (term.has-vars? t)
     #t)
   (define (term.match signature var other)
     (conditional-match (conforms-to? (signature-sort-graph signature)
                                      (generic-sort other)
                                      (var-sort-or-kind var))
                        (substitution var other)))
   (define (term.substitute signature var substitution)
     (define value (substitution-value substitution var))
     (if value
         value
         var))
   (define (term.vars var)
     (set var))])

(define (make-var varset symbol)
  (define sort-or-kind (lookup-var varset symbol))
  (and sort-or-kind
       (var (varset-sort-graph varset) symbol sort-or-kind)))

(module+ test
  (define a-varset
    (~> (empty-varset sorts)
        (add-var 'A-var 'A)
        (add-var 'B-var 'B)
        (add-var 'Zero-var 'Zero)
        (add-var 'Integer-var 'Integer)
        (add-var 'NonZeroInteger-var 'NonZeroInteger)
        (add-var 'StrangelyNamedVar 'Zero)))
  (define A-var (make-var a-varset 'A-var))
  (define B-var (make-var a-varset 'B-var))
  (define Zero-var (make-var a-varset 'Zero-var))
  (define Integer-var (make-var a-varset 'Integer-var))
  (define NonZeroInteger-var (make-var a-varset 'NonZeroInteger-var))
  (check-true (term.has-vars? A-var))
  (check-equal? (term.vars A-var) (set A-var))
  (check-equal? (term.key A-var) '*variable*)
  (check-true (term.has-vars? Zero-var))
  (check-equal? (term.vars Zero-var) (set Zero-var))
  (check-equal? (term.key Zero-var) '*variable*)
  (check-true (term.has-vars? Integer-var))
  (check-equal? (term.vars Integer-var) (set Integer-var))
  (check-equal? (term.key Integer-var) '*variable*)
  (check-single-match a-signature Zero-var 0
                      (substitution Zero-var 0))
  (check-single-match a-signature Integer-var 0
                      (substitution Integer-var 0))
  (check-no-match a-signature NonZeroInteger-var 0)
  (check-self-substitution a-signature Zero-var 0)
  (check-no-substitution a-signature Zero-var))

;
; Operator-defined patterns
; An op-pattern is a special case of an op-term that can contain
; variables.
;
(struct op-pattern op-term ()
  #:transparent
  #:methods gen:term
  [(define (term.sort t)
     (op-term-sort t))

   (define (term.key t)
     (op-term-op t))

   (define (term.has-vars? t)
     #t)

   (define/generic generic-match term.match)
   (define (term.match signature pattern term)
     (define (match-args p-args t-args substitution)
       (if (empty? p-args)
           (conditional-match substitution substitution)
           (in-generator
            (for ([sf (generic-match signature (first p-args) (first t-args))])
              (let ([sm (merge-substitutions substitution sf)])
                (when sm
                  (for ([s (match-args (rest p-args) (rest t-args) sm)])
                    (yield s))))))))
     (condd
      [(or (not (op-term? term))
           (not (equal? (op-term-op pattern) (op-term-op term))))
       no-match]
      #:do (define p-args (op-term-args pattern))
      #:do (define t-args (op-term-args term))
      [(not (equal? (length p-args) (length t-args)))
       no-match]
      [else
       (match-args p-args t-args empty-substitution)]))

   (define/generic generic-substitute term.substitute)
   (define (term.substitute signature pattern substitution)
     (make-term signature
                (op-term-op pattern)
                (for/list ([arg (op-term-args pattern)])
                  (generic-substitute signature arg substitution))))

   (define/generic generic-vars term.vars)
   (define (term.vars pattern)
     (foldl set-union (set) (map generic-vars (op-term-args pattern))))])

(module+ test
  (define a-one-var-pattern (make-term a-signature 'foo (list B-var)))
  (check-equal? (term.sort a-one-var-pattern) 'A)
  (check-true (term.has-vars? a-one-var-pattern))
  (check-equal? (term.vars a-one-var-pattern) (set B-var))
  (check-single-match a-signature a-one-var-pattern
                      (make-term a-signature 'foo (list a-B))
                      (substitution B-var a-B))
  (check-no-match a-signature a-one-var-pattern
                  (make-term a-signature 'foo (list an-A)))

  (define a-two-var-pattern (make-term a-signature 'foo (list A-var B-var)))
  (check-equal? (term.sort a-two-var-pattern) 'A)
  (check-true (term.has-vars? a-two-var-pattern))
  (check-equal? (term.vars a-two-var-pattern) (set A-var B-var))
  (check-single-match a-signature a-two-var-pattern
                      (make-term a-signature 'foo (list an-A a-B))
                      (merge-substitutions
                       (substitution B-var a-B)
                       (substitution A-var an-A)))
  (check-single-match a-signature a-two-var-pattern
                      (make-term a-signature 'foo (list a-B a-B))
                      (merge-substitutions
                       (substitution B-var a-B)
                       (substitution A-var a-B)))

  (define a-double-var-pattern (make-term a-signature 'foo (list B-var B-var)))
  (define foo0 (make-term a-signature 'foo empty))
  (check-equal? (term.sort a-double-var-pattern) 'A)
  (check-true (term.has-vars? a-double-var-pattern))
  (check-equal? (term.vars a-double-var-pattern) (set B-var))
  (check-single-match a-signature a-double-var-pattern
                      (make-term a-signature 'foo (list a-B a-B))
                      (substitution B-var a-B))
  (check-single-match a-signature a-double-var-pattern
                      (make-term a-signature 'foo (list foo0 foo0))
                      (substitution B-var foo0))
  (check-no-match a-signature a-double-var-pattern
                  (make-term a-signature 'foo (list a-B foo0)))
  (check-self-substitution a-signature
                           a-double-var-pattern
                           (make-term a-signature 'foo (list a-B a-B)))
  (check-no-substitution a-signature a-double-var-pattern))

;
; Make a variable or an operator-defined term. The result is a pattern
; if any of the arguments is a pattern.
;
(define (allowed-term? signature term)
  (cond
    [(op-term? term)
     (equal? (op-term-signature term) signature)]
    [(var? term)
     (equal? (var-sort-graph term) (signature-sort-graph signature))]
    [(term.builtin-type term)
     => (λ (t) (set-member? (signature-builtins signature) t))]
    [else
     (error "Unknown term type")]))

(define (make-term signature name args)
  (for ([arg args])
    (unless (allowed-term? signature arg)
      (error "argument term not compatible with signature")))
  (define rank (lookup-op signature name (map term.sort args)))
  (and rank
       (if (ormap term.has-vars? args)
           (op-pattern signature name args (cdr rank))
           (op-term signature name args (cdr rank)))))

(module+ test
  (define simple-sorts
    (~> (empty-sort-graph)
        (add-sort 'A) (add-sort 'B)
        (add-subsort-relation 'B 'A)))
  (define simple-signature
    (~> (empty-signature simple-sorts)
        (add-op 'foo empty 'B)
        (add-op 'foo (list 'B) 'A)
        (add-op 'foo (list 'A 'B) 'A)))
  ; Error: Argument term was made for a-signature
  (check-exn exn:fail? (thunk (make-term simple-signature 'foo (list a-B))))
  ; Error: variable from an incompatible varset
  (check-exn exn:fail? (thunk (make-term simple-signature 'foo (list B-var))))
  ; Error: integers are not allowed in simple-signature
  (check-exn exn:fail? (thunk (make-term simple-signature 'foo (list 1))))
  ; All arguments correct
  (let* ([a-B (make-term simple-signature 'foo empty)]
         [an-A (make-term simple-signature 'foo (list a-B))])
    (check-equal? (term.sort (make-term simple-signature 'foo (list an-A a-B)))
                  'A)))
