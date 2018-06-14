#lang racket

(provide
 (struct-out var)
 (contract-out
  [term?                  (any/c . -> . boolean?)]
  [term.sort              (term? . -> . sort-or-kind?)]
  [term.has-vars?         (term? . -> . boolean?)]
  [term.vars              (term? . -> . set?)]
  [term.key               (term? . -> . symbol?)]
  [term.builtin-type      (term? . -> . symbol?)]
  [term.op-and-args       (term? . -> . (values (or/c #f symbol?)
                                                (or/c #f list?)))]
  [term.in-signature      (term? signature? . -> . term?)]
  [substitution?          (any/c . -> . boolean?)]
  [empty-substitution     substitution?]
  [substitution-value     (substitution? symbol? . -> . term?)]
  [merge-substitutions    (substitution? substitution?  . -> . substitution?)]
  [term.match             (signature? term? term? . -> . sequence?)]
  [all-matches            (signature? term? term? . -> . list?)]
  [one-match              (signature? term? term? . -> . substitution?)]
  [term.substitute        (signature? term? substitution? . -> . term?)]
  [allowed-term?          (signature? term? . -> . boolean?)]
  [valid-term?            (signature? any/c . -> . boolean?)]
  [make-term              (signature? symbol? list? . -> . term?)]
  [make-term*             (signature? symbol? list? . -> . (or/c #f term?))]
  [make-var               ((signature? symbol?) (hash?) . ->* . (or/c #f var?))]
  [make-var-or-term       ((signature? symbol?) (hash?) . ->* . term?)]
  [make-uvar              (sort-graph? symbol? . -> . var?)]
  [make-unique-var        (sort-graph? symbol? sort? . -> . var?)]
  [display-vars           (set? output-port? . -> . void?)]
  [display-term-with-vars ((term? output-port?) (any/c) . ->* . void?)]
  [display-term           ((term? output-port?) (any/c) . ->* . void?)]
  [term->string           (term? . -> . string?)]))

(require "./lightweight-class.rkt"
         "./sorts.rkt"
         "./operators.rkt"
         "./builtins.rkt"
         "./condd.rkt"
         racket/generic
         racket/generator
         threading)

(module+ test
  (require rackunit racket/function)
  (define-syntax-rule (check-values-equal? a b)
    (check-equal? (call-with-values (thunk a) list)
                  (call-with-values (thunk b) list)))
  ; Define a simple sort graph and signature for testing
  (define sorts
    (~> rational-sorts
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
  [term.op-and-args term]
  [term.in-signature term signature]
  [term.match signature term other]
  [term.substitute signature term substitution]
  #:fast-defaults
  ([number?
    (define term.sort number-term.sort)
    (define term.builtin-type number-term.builtin-type)
    (define term.key term.builtin-type)
    (define (term.has-vars? x) #f)
    (define term.vars non-pattern-vars)
    (define (term.op-and-args x) (values #f #f))
    (define (term.in-signature term signature) term)
    (define term.match non-pattern-match)
    (define term.substitute non-pattern-substitute)]
   [symbol?
    (define (term.sort x) 'symbol)
    (define (term.builtin-type x) '*symbol*)
    (define term.key term.builtin-type)
    (define (term.has-vars? x) #f)
    (define term.vars non-pattern-vars)
    (define (term.op-and-args x) (values #f #f))
    (define (term.in-signature term signature) term)
    (define term.match non-pattern-match)
    (define term.substitute non-pattern-substitute)]
   [string?
    (define (term.sort x) 'string)
    (define (term.builtin-type x) '*string*)
    (define term.key term.builtin-type)
    (define (term.has-vars? x) #f)
    (define term.vars non-pattern-vars)
    (define (term.op-and-args x) (values #f #f))
    (define (term.in-signature term signature) term)
    (define term.match non-pattern-match)
    (define term.substitute non-pattern-substitute)])
  #:fallbacks
  [(define (term.builtin-type x) #f)
   (define (term.has-vars? x) #f)
   (define term.vars non-pattern-vars)
   (define (term.op-and-args x) (values #f #f))
   (define (term.in-signature term signature) term)
   (define term.match non-pattern-match)
   (define term.substitute non-pattern-substitute)])

(module+ test
  (check-equal? (term.sort 0) 'zero)
  (check-equal? (term.sort 1) 'ℕnz)
  (check-equal? (term.sort -1) 'ℤnz)
  (check-equal? (term.sort 1/2) 'ℚp)
  (check-equal? (term.sort -1/2) 'ℚnz)
  (check-equal? (term.builtin-type 0) '*integer*)
  (check-equal? (term.builtin-type 1/2) '*rational*)
  (check-equal? (term.key 0) '*integer*)
  (check-equal? (term.key 1/2) '*rational*)
  (check-false (term.has-vars? 1))
  (check-values-equal? (term.op-and-args 1) (values #f #f))
  (check-equal? (term.in-signature 1 a-signature) 1)
  (check-equal? (term.sort 'foo) 'symbol)
  (check-equal? (term.builtin-type 'foo) '*symbol*)
  (check-equal? (term.key 'foo) '*symbol*)
  (check-false (term.has-vars? 'foo))
  (check-values-equal? (term.op-and-args 'foo) (values #f #f))
  (check-equal? (term.in-signature 'foo a-signature) 'foo)
  (check-equal? (term.sort "foo") 'string)
  (check-equal? (term.builtin-type "foo") '*string*)
  (check-equal? (term.key "foo") '*string*)
  (check-false (term.has-vars? "foo"))
  (check-values-equal? (term.op-and-args "foo") (values #f #f))
  (check-equal? (term.in-signature "foo" a-signature) "foo"))

;
; Substitutions are varname->term hashes describing a match.
; Combining two substitutions that are contradictory leads to
; a non-match signalled by the special value #f.
;
(define (substitution? x)
  (hash? x))

(define empty-substitution (hash))

(define (substitution varname term)
  (hash varname term))

(define (merge-substitutions s1 s2)
  (if (not s2)
      #f
      (for/fold ([s-acc s1])
                ([(varname value) s2])
        #:break (not s-acc)
        (if (and (hash-has-key? s-acc varname)
                 (not (equal? (hash-ref s-acc varname) value)))
            #f
            (hash-set s-acc varname value)))))

(define (substitution-value substitution varname)
  (hash-ref substitution varname #f))

(module+ test
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

(define (one-match signature pattern term)
  (define matches (all-matches signature pattern term))
  (when (empty? matches)
    (error "no match"))
  (unless (equal? (length matches) 1)
    (error (format "more than one match: ~a" matches)))
  (first matches))

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
                             (substitution 'StrangelyNamedVar 0))
            pattern)))

(module+ test
  (check-single-match a-signature 1 1 empty-substitution)
  (check-no-match a-signature 0 1))

;
; Readable output for terms
;
(define (display-vars vars port)
  (unless (set-empty? vars)
    (display
     (string-join (for/list ([var (in-set vars)])
                    (format "[~s ~s]" (var-name var) (var-sort var)))
                  " "
                  #:before-first "#:vars ("
                  #:after-last ")  ")
     port)))

(define (display-term term port [mode #f])
  (cond
    [(or (op-term? term)
         (op-pattern? term))
     (if (empty? (op-term-args term))
         (display (op-term-op term) port)
         (begin
           (display "(" port)
           (display (op-term-op term) port)
           (for ([arg (op-term-args term)])
             (display " " port)
             (display-term arg port mode))
           (display ")" port)))]
    [(var? term)
     (display (var-name term) port)]
    [(symbol? term)
     (begin
       (display "'" port)
       (display term port))]
    [(string? term)
     (write term port)]
    [else
     (display term port)]))

(define (display-term-with-vars term port [mode #f])
  (define sort (term.sort term))
  (if (kind? sort)
      (begin
        (display "[" port)
        (display (set-first sort) port)
        (display "]" port))
      (display sort port))
  (display ":" port)
  (define vars (term.vars term))
  (if (set-empty? vars)
      (display-term term port mode)
      (begin
        (display "(pattern " port)
        (display-vars vars port)
        (display-term term port)
        (display ")" port))))

(define (term->string term)
    (let ([o (open-output-string)])
      (display-term-with-vars term o)
      (get-output-string o)))

(module+ test
  (check-equal? (term->string 2) "ℕnz:2")
  (check-equal? (term->string 'foo) "symbol:'foo")
  (check-equal? (term->string "foo") "string:\"foo\"")
  (check-equal? (term->string (make-term a-signature 'a-B empty))
                "B:a-B")
  (check-equal? (term->string
                 (make-term a-signature 'foo
                            (list (make-term a-signature 'an-A empty))))
                "[A]:(foo an-A)"))

;
; Operator-defined terms
;

(struct op-term (signature op args sort)
  #:transparent
  #:methods gen:term
  [(define (term.sort t)
     (op-term-sort t))
   (define (term.key t)
     (op-term-op t))
   (define (term.op-and-args t)
     (values (op-term-op t) (op-term-args t)))
   (define (term.in-signature term signature)
     (op-term-in-signature term signature))]
  #:methods gen:custom-write
  [(define write-proc display-term-with-vars)])

(module+ test
  (define an-A (make-term a-signature 'an-A empty))
  (check-equal? (term.sort an-A) 'A)
  (check-equal? (term.key an-A) 'an-A)
  (check-false (term.has-vars? an-A))
  (check-values-equal? (term.op-and-args an-A) (values 'an-A empty))
  (define a-B (make-term a-signature 'a-B empty))
  (check-equal? (term.sort a-B) 'B)
  (check-equal? (term.key a-B) 'a-B)
  (check-false (term.has-vars? a-B))
  (check-values-equal? (term.op-and-args a-B) (values 'a-B empty))
  (define an-X (make-term a-signature 'an-X empty))
  (check-equal? (term.sort an-X) 'X)
  (check-equal? (term.key an-X) 'an-X)
  (check-false (term.has-vars? an-X))
  (check-values-equal? (term.op-and-args an-X) (values 'an-X empty))
  (define a-Y (make-term a-signature 'a-Y empty))
  (check-equal? (term.sort a-Y) 'Y)
  (check-equal? (term.key a-Y) 'a-Y)
  (check-false (term.has-vars? a-Y))
  (check-values-equal? (term.op-and-args a-Y) (values 'a-Y empty))
 
  (check-equal? (term.sort (make-term a-signature 'foo empty)) 'B)
  (check-equal? (term.sort (make-term a-signature 'foo (list a-B))) 'A)
  (check-equal? (term.sort (make-term a-signature 'foo (list an-A a-B))) 'A)
  (check-equal? (term.sort (make-term a-signature 'foo (list a-B a-B))) 'A)
  (check-equal? (term.sort (make-term a-signature 'foo (list an-A)))
                (kind sorts 'A))
  (check-equal? (term.sort (make-term a-signature 'foo (list an-A an-A)))
                (kind sorts 'A))
  (check-equal? (term.key (make-term a-signature 'foo empty)) 'foo)
  (check-exn exn:fail? (thunk (make-term a-signature 'foo (list an-X))))
  (check-values-equal? (term.op-and-args(make-term a-signature
                                                   'foo (list an-A)))
                       (values 'foo (list an-A)))

  (check-single-match a-signature
                      (make-term a-signature 'foo (list a-B))
                      (make-term a-signature 'foo (list a-B))
                      empty-substitution)
  (check-no-match a-signature
                  (make-term a-signature 'foo (list a-B))
                  (make-term a-signature 'foo empty)))

;
; Variables
;
(struct var (sort-graph name sort)
  #:transparent
  #:methods gen:term
  [(define/generic generic-sort term.sort)
   (define (term.sort t)
     (var-sort t))
   (define (term.key t)
     '*variable*)
   (define (term.has-vars? t)
     #t)
   (define (term.match signature var other)
     (conditional-match (conforms-to? (signature-sort-graph signature)
                                      (generic-sort other)
                                      (var-sort var))
                        (substitution (var-name var) other)))
   (define (term.substitute signature var substitution)
     (define value (substitution-value substitution (var-name var)))
     (if value
         value
         var))
   (define (term.vars var)
     (set var))
   (define (term.in-signature x signature)
     (define sorts (signature-sort-graph signature))
     (var sorts (var-name x) (var-sort x)))]
  #:methods gen:custom-write
  [(define write-proc display-term-with-vars)])


(define (make-var signature symbol [local-vars #f])
  (define sort (or (and local-vars
                        (hash-ref local-vars symbol #f))
                   (lookup-var signature symbol)))
  (and sort
       (var (signature-sort-graph signature) symbol sort)))

(define (make-uvar sort-graph symbol)
  (var sort-graph symbol #f))

(define (make-unique-var sort-graph symbol sort)
  (var sort-graph (gensym symbol) sort))

(module+ test
  (define a-var-signature
    (~> (empty-signature sorts)
        (add-var 'A-var 'A)
        (add-var 'B-var 'B)
        (add-var 'Zero-var 'zero)
        (add-var 'Integer-var 'ℤ)
        (add-var 'NonZeroInteger-var 'ℤnz)
        (add-var 'StrangelyNamedVar 'zero)))
  (define A-var (make-var a-var-signature 'A-var))
  (define B-var (make-var a-var-signature 'B-var))
  (define Zero-var (make-var a-var-signature 'Zero-var))
  (define Integer-var (make-var a-var-signature 'Integer-var))
  (define NonZeroInteger-var (make-var a-var-signature 'NonZeroInteger-var))
  (define U-var (make-uvar sorts 'U-var))
  (check-true (term.has-vars? A-var))
  (check-equal? (term.vars A-var) (set A-var))
  (check-equal? (term.key A-var) '*variable*)
  (check-true (term.has-vars? Zero-var))
  (check-equal? (term.vars Zero-var) (set Zero-var))
  (check-equal? (term.key Zero-var) '*variable*)
  (check-true (term.has-vars? Integer-var))
  (check-equal? (term.vars Integer-var) (set Integer-var))
  (check-equal? (term.key Integer-var) '*variable*)
  (check-true (term.has-vars? U-var))
  (check-equal? (term.vars U-var) (set U-var))
  (check-equal? (term.key U-var) '*variable*)
  (check-single-match a-signature Zero-var 0
                      (substitution 'Zero-var 0))
  (check-single-match a-signature Integer-var 0
                      (substitution 'Integer-var 0))
  (check-no-match a-signature NonZeroInteger-var 0)
  (check-self-substitution a-signature Zero-var 0)
  (check-no-substitution a-signature Zero-var)
  (check-single-match a-signature U-var 0 (substitution 'U-var 0))
  (check-self-substitution a-signature U-var 0))

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

   (define (term.op-and-args t)
     (values (op-term-op t) (op-term-args t)))

   (define (term.in-signature term signature)
     (op-term-in-signature term signature))

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
     (make-term* signature
                 (op-term-op pattern)
                 (for/list ([arg (op-term-args pattern)])
                   (generic-substitute signature arg substitution))))

   (define/generic generic-vars term.vars)
   (define (term.vars pattern)
     (foldl set-union (set) (map generic-vars (op-term-args pattern))))]
  #:methods gen:custom-write
  [(define write-proc display-term-with-vars)])

(module+ test
  (define a-one-var-pattern (make-term a-signature 'foo (list B-var)))
  (check-equal? (term.sort a-one-var-pattern) 'A)
  (check-values-equal? (term.op-and-args a-one-var-pattern)
                       (values 'foo (list B-var)))
  (check-true (term.has-vars? a-one-var-pattern))
  (check-equal? (term.vars a-one-var-pattern) (set B-var))
  (check-single-match a-signature a-one-var-pattern
                      (make-term a-signature 'foo (list a-B))
                      (substitution 'B-var a-B))
  (check-no-match a-signature a-one-var-pattern
                  (make-term a-signature 'foo (list an-A)))

  (define a-two-var-pattern (make-term a-signature 'foo (list A-var B-var)))
  (check-equal? (term.sort a-two-var-pattern) 'A)
  (check-values-equal? (term.op-and-args a-two-var-pattern)
                       (values 'foo (list A-var B-var)))
  (check-true (term.has-vars? a-two-var-pattern))
  (check-equal? (term.vars a-two-var-pattern) (set A-var B-var))
  (check-single-match a-signature a-two-var-pattern
                      (make-term a-signature 'foo (list an-A a-B))
                      (merge-substitutions
                       (substitution 'B-var a-B)
                       (substitution 'A-var an-A)))
  (check-single-match a-signature a-two-var-pattern
                      (make-term a-signature 'foo (list a-B a-B))
                      (merge-substitutions
                       (substitution 'B-var a-B)
                       (substitution 'A-var a-B)))

  (define a-double-var-pattern (make-term a-signature 'foo (list B-var B-var)))
  (define foo0 (make-term a-signature 'foo empty))
  (check-equal? (term.sort a-double-var-pattern) 'A)
  (check-values-equal? (term.op-and-args a-double-var-pattern)
                       (values 'foo (list B-var B-var)))
  (check-true (term.has-vars? a-double-var-pattern))
  (check-equal? (term.vars a-double-var-pattern) (set B-var))
  (check-single-match a-signature a-double-var-pattern
                      (make-term a-signature 'foo (list a-B a-B))
                      (substitution 'B-var a-B))
  (check-single-match a-signature a-double-var-pattern
                      (make-term a-signature 'foo (list foo0 foo0))
                      (substitution 'B-var foo0))
  (check-no-match a-signature a-double-var-pattern
                  (make-term a-signature 'foo (list a-B foo0)))
  (check-self-substitution a-signature
                           a-double-var-pattern
                           (make-term a-signature 'foo (list a-B a-B)))
  (check-no-substitution a-signature a-double-var-pattern)

  (define Uvar-X (make-uvar truth-sorts 'X))
  (define Uvar-Y (make-uvar truth-sorts 'Y))
  (define true-term (make-term truth-signature 'true empty))
  (define an-equality-pattern (make-term truth-signature
                                         '_== (list Uvar-X Uvar-Y)))
  (define an-equality-term (make-term truth-signature
                                      '_== (list true-term true-term)))
  (check-single-match truth-signature an-equality-pattern an-equality-term
                      (merge-substitutions
                       (substitution 'X true-term)
                       (substitution 'Y true-term))))

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

(define (valid-term? signature term)
    (define-values (op args) (term.op-and-args term))
    (and (term? term)
         (allowed-term? signature term)
         (or (not op)
             (andmap (λ (arg) (valid-term? signature arg)) args))))

; A minimal make-term without error checking
(define (make-term* signature name args)
  (define rank (lookup-op signature name (map term.sort args)))
  (and rank
       (if (ormap term.has-vars? args)
           (op-pattern signature name args (cdr rank))
           (op-term signature name args (cdr rank)))))

; The safe-to-use make-term for general use
(define (make-term signature name args)
  (for ([arg args])
    (unless (allowed-term? signature arg)
      (error (format "argument ~a not compatible with signature" arg))))
  (or (make-term* signature name args)
      (error (if (empty? args)
                 (format "no operator or var definition for ~s" name )
                 (format "no operator definition for ~s~s"
                         name (map term.sort args))))))

(define (make-var-or-term signature name [local-vars #f])
  (or (make-var signature name local-vars)
      (make-term signature name empty)))

; Adapt a term to a larger signature. Used for merging contexts.
(define (op-term-in-signature term signature)
  (define-values (op args) (term.op-and-args term))
  (make-term* signature op
              (map (λ (t) (term.in-signature t signature)) args)))

(module+ test
  (define simple-sorts
    (~> empty-sort-graph
        (add-sort 'A) (add-sort 'B)
        (add-subsort-relation 'B 'A)))
  (define simple-signature
    (~> (empty-signature simple-sorts)
        (add-op 'foo empty 'B)
        (add-op 'foo (list 'B) 'A)
        (add-op 'foo (list 'A 'B) 'B)))
  ; Error: Argument term was made for a-signature
  (check-exn exn:fail? (thunk (make-term simple-signature 'foo (list a-B))))
  ; Error: integers are not allowed in simple-signature
  (check-exn exn:fail? (thunk (make-term simple-signature 'foo (list 1))))
  ; All arguments correct
  (define ss-a-B (make-term simple-signature 'foo empty))
  (define ss-an-A (make-term simple-signature 'foo (list ss-a-B)))
  (define ss-test (make-term simple-signature 'foo (list ss-an-A ss-a-B)))

  (check-equal? (term.sort ss-test) 'B)
  (check-equal? (term->string ss-test) "B:(foo (foo foo) foo)")
  (check-true (valid-term? simple-signature ss-a-B))
  (check-true (valid-term? simple-signature ss-an-A))
  (check-true (valid-term? simple-signature ss-test))

  (define larger-signature
    (~> simple-signature
        (add-op 'foo (list 'A 'A) 'A)))
  (check-true (valid-term? larger-signature 
                           (term.in-signature ss-a-B larger-signature)))
  (check-true (valid-term? larger-signature 
                           (term.in-signature ss-an-A larger-signature)))
  (check-true (valid-term? larger-signature 
                           (term.in-signature ss-test larger-signature))))

