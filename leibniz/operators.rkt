#lang racket

(provide
 (struct-out signature)
 (contract-out
  [regular?         (signature? . -> . boolean?)]
  [non-regular-op-example
                    (signature? . -> .
                      (or/c #f
                            (list/c symbol?
                                    (listof symbol?)
                                    (listof symbol?))))]
  [empty-signature  ((sort-graph?) (#:builtins set?) . ->* . signature?)]
  [add-op           ((signature? symbol? (listof sort-constraint?) sort?) (#:meta any/c)
                     . ->* . signature?)]
  [add-var          (signature? symbol? sort? . -> . signature?)]
  [remove-vars      (signature? . -> . signature?)]
  [merge-signatures (signature? signature? (or/c #f sort-graph?)
                     . -> . signature?)]
  [lookup-op        (signature? symbol? (listof (or/c #f sort-or-kind?))
                     . -> .
                     (or/c #f (cons/c (listof sort-constraint?) sort-or-kind?)))]
  [lookup-op-meta   (signature? symbol? (listof (or/c #f sort-or-kind?))
                     . -> .
                     (or/c #f (list/c (listof sort-constraint?) sort-or-kind? any/c)))]
  [lookup-op-rank-list (signature? symbol? (listof (or/c #f sort-or-kind?))
                        . -> .
                        (listof (list/c (listof sort-constraint?) sort-or-kind? any/c)))]
  [lookup-var        (signature? symbol? . -> . (or/c #f sort?))]
  [ops-by-kind-arity (signature? . -> . (sequence/c symbol? list?))]
  [all-ops           (signature? . -> . (sequence/c symbol? pair? any/c))]
  [all-vars          (signature? . -> . hash?)]
  [builtin-term-types (signature? . -> . (set/c symbol?))]
  [display-signature (signature? natural-number/c output-port? . -> . void?)]))

(require "./lightweight-class.rkt"
         "./sorts.rkt"
         threading
         racket/hash
         racket/generator)

(module+ test
  (require rackunit racket/function)
  ; Define a simple sort graph for testing
  (define sorts
    (~> empty-sort-graph
        (add-sort 'A) (add-sort 'B)
        (add-subsort-relation 'B 'A)
        (add-sort 'X) (add-sort 'Y)
        (add-subsort-relation 'Y 'X))))

;
; Utilities
;
(define (cartesian-product seqs)
  (in-generator
   (if (empty? seqs)
       (yield empty)
       (for* ([s (first seqs)]
              [ss (cartesian-product (rest seqs))])
         (yield (cons s ss))))))

(module+ test
  (check-equal? (sequence->list (cartesian-product '((1 2) (a b))))
                '((1 a) (1 b) (2 a) (2 b)))
  (check-equal? (sequence->list (cartesian-product '((1) (#t) ("x"))))
                '((1 #t "x")))
  (check-equal? (sequence->list (cartesian-product '((1) ())))
                empty)
  (check-equal? (sequence->list (cartesian-product empty))
                (list empty)))

;
; Operators and signatures
;
; Following the terminology of "Order-Sorted Algebra I" by Goguen and
; Meseguer, an operator is defined by a symbol (its name), its arity
; (a list of sort constraints, one for each argument), and its sort
; (the sort of the term defined by the operator). The combination
; (arity, sort) is called the rank of the operator.  A signature is a
; set of operators that respect the criterion of monotonicity: if two
; operator definitions have the same name and the same number of
; arguments, and all argument sorts of the first are subsorts of the
; corresponding argument sorts of the second, then the sort of the
; first must be a subsort of the sort of the second.
;
; All these definitions are stored in a few data structures:
;
;  - A signature is a hash from symbols to operators.
;
;  - An operator is a hash from kind-arities to sorted rank lists
;
;  - A sorted rank list is a list of ranks that is kept partially
;    sorted with respect to subsort relations as new ranks are added to
;    it.
;
; The only data structure for use by client code is the signature.
; Operators and sorted rank lists are implementation details.
;
; A kind-arity is obtained from an arity by substituting each sort by
; its kind. Kind-arities matter because operator definitions with
; different kind-arities are completely distinct, even if they share
; the same name, whereas definitions with the same kind-arity are
; subject to monotonicity and regularity (a stronger condition that
; ensures some nice properties, see the paper for the
; details). Kind-arities are represented as lists of kinds.
;
; Monotonicity is easy to check incrementally during the assembly of a
; signature, and any attempt to make a non-monotonic one throws an
; execption. Regularity cannot be verified in the same way, because
; a non-regular signature can become regular by adding more
; operator definitions. It is therefore the responsibility of client
; code to check a signature for regularity before relying on this
; property.

;
; List of ranks partially sorted by arity
;
; For a regular set of ranks, scanning such a partially
; sorted list up to the first matching arity always yields
; the least rank and thus the least sort. If the ranks are
; non-regular, the lookup yields a rank/sort that is one
; possible "least" choice among several.
;
(define-class sorted-ranks

  (field sort-graph k-rank ranks)
  ; sort-graph: the sort graph everything is based on
  ; k-rank: the kind-arity plus kind-of-sort that is common
  ;         to all definitions in this rank list
  ; ranks: the partially sorted list of ranks with attached metadata

  (define (check-kinds k-arity k-sort)
    (match-let ([(list a s m) k-rank])
      (and (equal? k-arity a)
           (equal? k-sort s))))

  (define (is-subarity? arity1 arity2)
    (and (= (length arity1) (length arity2))
         (for/and ([s1 arity1] [s2 arity2])
           (conforms-to? sort-graph s1 s2))))

  (define (add arity sort meta)

    ; Adding a new rank requires
    ; (1) finding the right place to insert it
    ; (2) constructing the new rank list
    ;
    ; (add*) performs (1), keeping a list of already-inspected
    ; ranks (prefix) in reverse order for later use by (insert-rank),
    ; which performs (2).

    (define (insert-rank prefix ranks)
      (sorted-ranks sort-graph k-rank
                    (for/fold ([ranks (cons (list arity sort meta) ranks)])
                              ([r prefix])
                      (cons r ranks))))

    (define (add* prefix ranks)
      (if (empty? ranks)
          (insert-rank prefix ranks)
          (match-let* ([(and r (list a s m)) (first ranks)])
            (cond
              [(equal? arity a)
               ; The new arity is already present in the list.
               (if (equal? sort s)
                   this
                   (error (format "new value sort ~a for arity ~a differs from ~a in previous declaration"
                                  sort arity s)))]
              [(is-subarity? arity a)
               ; We hit a higher arity, so we must insert the new
               ; one before it. We must also check the sorts
               ; for ensuring monotonicity.
               (if (is-subsort? sort-graph sort s)
                   (insert-rank prefix ranks)
                   (error (format  "operator not monotonic after adding ~a : ~a to ~a"
                                   arity sort ranks)))]
              [(and (is-subarity? a arity)
                    (not (is-subsort? sort-graph s sort)))
               ; We hit a sub-arity of the one we want to add,
               ; but the corresponding sort is not a sub-sort,
               ; so we signal a monotonicity error.
               (error (format  "operator not monotonic after adding ~a : ~a to ~a"
                               arity sort ranks))]
              [else
               ; We get here in two situations:
               ; 1) The new arity is unrelated to the current one.
               ; 2) The new arity is higher than the current one
               ;    AND monotonicity has been verified.
               ; We thus move on to the next entry in the rank list.
               (add* (cons r prefix) (rest ranks))]))))

    (add* empty ranks))

  (define (matching-ranks-for-arity arity)
    (filter (λ (r) (is-subarity? arity (first r))) ranks))

  (define (smallest-rank-for-arity arity)
    (define after (dropf ranks
                         (λ (r) (not (is-subarity? arity (first r))))))
    (if (empty? after)
        k-rank
        (car after)))

  (define (monotonic-ranks?)
    ; Monotonicity of the rank lists is guaranteed during construction.
    ; This method is only provided for testing.
    (define (test-first r1 rs)
      (for/and ([r2 rs]
                #:when (is-subarity? (first r1) (first r2)))
        (is-subsort? sort-graph (second r1) (second r2))))
    (define (monotonic* rs)
      (define f (first rs))
      (define r (rest rs))
      (or (empty? r)
          (and (test-first f r)
               (monotonic* r))))
    (monotonic* ranks))

  (define (non-regularity-example-ranks)
    ; Do a brute-force search over all argument sorts that have
    ; more than one immediate supersort, and yield those for which
    ; there is no unique least value sort.
    (in-generator #:arity 2
     (for ([arg-sorts
            (cartesian-product
             (for/list ([c (first k-rank)])
               (for/set ([s (in-set (conforming-sorts sort-graph c))]
                         #:when (has-multiple-supersorts? sort-graph s))
                 s)))])
       (define value-sorts (map second (matching-ranks-for-arity arg-sorts)))
       (unless (or (< (length value-sorts) 2)
                   (for/and ([vs (rest value-sorts)])
                     (is-subsort? sort-graph (first value-sorts) vs)))
         (yield arg-sorts value-sorts)))))
  
  (define (regular-rank-list?)
    (not (for/first ([(arg-sorts value-sorts) (non-regularity-example-ranks)])
           #t)))

  (define (all-ranks-for-k-arity)
    ranks))

(define (empty-sorted-ranks sort-graph k-arity k-sort)
  (define k-rank (list k-arity k-sort #f))
  (sorted-ranks sort-graph k-rank empty))

(module+ test

  (define a-sr-1
    (~> (empty-sorted-ranks sorts (list (kind sorts 'A)) (kind sorts 'A))
        (add (list 'A) 'A #f)
        (add (list 'B) 'B #f)))

  (define a-sr-2
    (~> (empty-sorted-ranks sorts (list (kind sorts 'X)) (kind sorts 'X))
        (add (list 'Y) 'Y #f)))

  (define a-sr-3
    (~> (empty-sorted-ranks sorts (list (kind sorts 'A) (kind sorts 'X))
                            (kind sorts 'A))
        (add (list 'A 'X) 'A #f)
        (add (list 'B 'Y) 'B #f)))

  (check-true (is-subarity? a-sr-1 (list 'B) (list 'A)))
  (check-true (is-subarity? a-sr-1 (list 'A) (list 'A)))
  (check-true (is-subarity? a-sr-1 (list 'B) (list 'B)))
  (check-false (is-subarity? a-sr-1 (list 'X) (list 'B)))
  (check-false (is-subarity? a-sr-1 (list 'A) (list 'X)))
  (check-false (is-subarity? a-sr-1 (list 'A) (list 'B)))
  (check-false (is-subarity? a-sr-1 (list 'B 'B) (list 'B)))

  (check-equal? (matching-ranks-for-arity a-sr-1 (list 'A))
                (list (list (list 'A) 'A #f)))
  (check-equal? (matching-ranks-for-arity a-sr-1 (list 'B))
                (list (list (list 'B) 'B #f) (list (list 'A) 'A #f)))
  (check-equal? (matching-ranks-for-arity a-sr-2 (list 'Y))
                (list (list (list 'Y) 'Y #f)))
  (check-equal? (matching-ranks-for-arity a-sr-2 (list 'X)) empty)

  (check-equal? (smallest-rank-for-arity a-sr-1 (list 'A)) (list (list 'A) 'A #f))
  (check-equal? (smallest-rank-for-arity a-sr-1 (list 'B)) (list (list 'B) 'B #f))
  (check-equal? (smallest-rank-for-arity a-sr-2 (list 'Y)) (list (list 'Y) 'Y #f))
  (check-equal? (smallest-rank-for-arity a-sr-2 (list 'X))
                (list (list (set 'X 'Y)) (set 'X 'Y) #f))

  (check-true (monotonic-ranks? a-sr-1))
  (check-true (monotonic-ranks? a-sr-2))
  (check-true (monotonic-ranks? a-sr-3))
  (check-true (regular-rank-list? a-sr-1))
  (check-true (regular-rank-list? a-sr-2))
  (check-true (regular-rank-list? a-sr-3))

  ; Try to make a non-monotonic rank list
  (check-exn exn:fail?
             (thunk (~> (empty-sorted-ranks sorts
                                            (list (kind sorts 'A))
                                            (kind sorts 'A))
                        (add (list 'A) 'B #f)
                        (add (list 'B) 'A #f))))

  ; Make a non-regular rank list, then add a rank to make it regular
  (let* ([sorts (~> empty-sort-graph
                    (add-sort 'A) (add-sort 'B) (add-sort 'C)
                    (add-subsort-relation 'A 'B)
                    (add-subsort-relation 'A 'C))]
         [srl (~> (empty-sorted-ranks sorts
                                      (list (kind sorts 'A))
                                      (kind sorts 'A))
                  (add (list 'B) 'B #f)
                  (add (list 'C) 'C #f))])
    (check-false (regular-rank-list? srl))
    (for ([(arg-sorts value-sorts) (non-regularity-example-ranks srl)])
      (check-equal? arg-sorts '(A))
      (check-equal? value-sorts '(B C)))
    (check-true (regular-rank-list? (~> srl
                                        (add (list 'A) 'A #f))))))

;
; Operators
;
(define-class operator

  (field sort-graph ranks-by-k-arity)
  ; sort-graph: the sort graph everything is based on
  ; ranks-by-k-arity: a hash mapping kind-arities to
  ;                   sorted rank lists

  (define (kind-arity arity)
    (map (λ (sc) (kind-constraint sort-graph sc)) arity))

  (define (add-rank arity sort meta)
    (define k-arity (kind-arity arity))
    (define k-sort (kind sort-graph sort))
    (define (update ranks)
      (unless (check-kinds ranks k-arity k-sort)
        (define formatted-ranks
          (for/list ([triple (all-ranks-for-k-arity ranks)])
            (format "  ~s -> ~a" (first triple) (second triple))))
        (error (format "adding rank ~s -> ~a makes operator non-monotonic\nPreviously defined ranks:\n~a"
                       arity sort
                       (string-join formatted-ranks "\n"))))
      (add ranks arity sort meta))
    (operator sort-graph
              (hash-update ranks-by-k-arity k-arity update
                           (thunk (empty-sorted-ranks sort-graph
                                                      k-arity k-sort)))))
  
  (define (lookup-rank arity)
    (define k-arity (kind-arity arity))
    (and~> (hash-ref ranks-by-k-arity k-arity #f)
           (smallest-rank-for-arity arity)))

  (define (lookup-rank-list arity)
    (define k-arity (kind-arity arity))
    (all-ranks-for-k-arity (hash-ref ranks-by-k-arity k-arity #f)))

  (define (monotonic-op?)
    (for/and ([sorted-ranks (in-hash-values ranks-by-k-arity)])
      (monotonic-ranks? sorted-ranks)))

  (define (regular-op?)
    (for/and ([srl (hash-values ranks-by-k-arity)])
      (regular-rank-list? srl)))

  (define (non-regularity-examples-op)
    (in-generator #:arity 2
      (for* ([srl (hash-values ranks-by-k-arity)]
             [(arg-sorts value-sorts) (non-regularity-example-ranks srl)])
        (yield arg-sorts value-sorts))))

  (define (all-k-arities)
    (hash-keys ranks-by-k-arity))

  (define (all-ranks)
    (in-generator
     (for* ([sorted-ranks (in-hash-values ranks-by-k-arity)]
            [rank (all-ranks-for-k-arity sorted-ranks)])
       (yield rank)))))

(define (empty-operator sort-graph)
  (operator sort-graph (hash)))

(module+ test

  (define an-op
    (~> (empty-operator sorts)
        (add-rank (list 'A) 'A #f)
        (add-rank (list 'B) 'B #f)
        (add-rank (list 'Y) 'Y #f)))

  (check-equal? (dict-count (operator-ranks-by-k-arity an-op)) 2)
  (check-equal? (lookup-rank an-op (list 'A)) (list (list 'A) 'A #f))
  (check-equal? (lookup-rank an-op (list 'B)) (list (list 'B) 'B #f))
  (check-equal? (lookup-rank an-op (list 'Y)) (list (list 'Y) 'Y #f))
  (check-equal? (lookup-rank an-op (list 'X))
                (list (list (set 'X 'Y)) (set 'X 'Y) #f))
  (check-equal? (for/set ([r (all-ranks an-op)]) r)
                (set (list (list 'A) 'A #f)
                     (list (list 'B) 'B #f)
                     (list (list 'Y) 'Y #f)))
  (check-true (regular-op? an-op))

  ; Try to make non-monotonic operators
  (check-exn exn:fail?
             (thunk (~> (empty-operator sorts)
                        (add-rank (list 'A) 'B #f)
                        (add-rank (list 'B) 'A #f))))
  (check-exn exn:fail?
             (thunk (~> (empty-operator sorts)
                        (add-rank (list 'A) 'B #f)
                        (add-rank (list 'B) 'X #f))))

  ; Make a non-regular operator, then add a rank to make it regular
  (let* ([sorts (~> empty-sort-graph
                    (add-sort 'A) (add-sort 'B) (add-sort 'C)
                    (add-subsort-relation 'A 'B)
                    (add-subsort-relation 'A 'C))]
         [op (~> (empty-operator sorts)
                 (add-rank (list 'B) 'B #f)
                 (add-rank (list 'C) 'C #f))])
    (check-false (regular-op? op))
    (for ([(arg-sorts value-sorts) (non-regularity-examples-op op)])
      (check-equal? arg-sorts '(A))
      (check-equal? value-sorts '(B C)))
    (check-true (regular-op? (~> op (add-rank (list 'A) 'A #f))))))

;
; Signatures
;
(define-class signature

  (field sort-graph optimized-sort-graph operators vars builtins)
  ; sort-graph: the sort graph everything is based on
  ; optimized-sort-graph: sort-graph optimized for subsort lookup
  ; operators: a hash mapping operator names (symbols) to operators
  ; builtins: the builtin special term types included in this signature
  ;           (a set of symbols)

  #:write-proc *display*

  (define (add-op* symbol arity sort meta)
    (when (and (empty? arity)
               (lookup-var symbol))
      (error (format "~a already defined as a var" symbol)))
    (for ([arg arity])
      (validate-sort-constraint sort-graph arg))
    (validate-sort sort-graph sort)
    (with-handlers ([exn:fail?
                     (λ (exn) (error (format "op ~a: ~a "
                                             symbol
                                             (exn-message exn))))])
      (let ([ops (hash-update operators symbol
                              (λ (op) (add-rank op arity sort meta))
                              (thunk (add-rank (empty-operator optimized-sort-graph)
                                               arity sort meta)))])
        (signature sort-graph optimized-sort-graph ops vars builtins))))

  (define (add-var symbol sort)
    (when (lookup-op symbol empty)
      (error (format "~a already defined as an operator" symbol)))
    (define existing-sort (lookup-var symbol))
    (when (and existing-sort
               (not (equal? sort existing-sort)))
      (error (format "var ~a already defined with sort ~a" symbol (lookup-var symbol))))
    (signature sort-graph optimized-sort-graph operators
               (hash-set vars symbol sort)
               builtins))

  (define (remove-vars)
    (signature sort-graph optimized-sort-graph operators
               (hash)
               builtins))

  (define (lookup-op-meta symbol arity)
    (define op (hash-ref operators symbol #f))
    (or
     (and op
          (lookup-rank op arity))
     ; A somewhat hacky special case: if the signature includes the
     ; *truth* builtin, it includes == for equality of arbitrary terms.
     (and (set-member? builtins '*truth*)
          (equal? symbol '_==)
          (= (length arity) 2)
          '((#f #f) boolean #f))))

  (define (lookup-op symbol arity)
    (define rank (lookup-op-meta symbol arity))
    (and rank
         (cons (first rank) (second rank))))

  (define (lookup-op-rank-list symbol arity)
    (define op (hash-ref operators symbol #f))
    (lookup-rank-list op arity))

  (define (lookup-var symbol)
    (hash-ref vars symbol #f))

  (define (regular?)
    (for/and ([op (hash-values operators)])
      (regular-op? op)))

  (define (non-regular-op-example)
    (for*/first ([(op-symbol op) operators]
           [(arg-sorts value-sorts) (non-regularity-examples-op op)])
      (list op-symbol arg-sorts value-sorts)))

  (define (ops-by-kind-arity)
    (in-generator #:arity 2
     (for* ([(symbol op) operators]
            [k-arity (all-k-arities op)])
       (yield symbol k-arity))))

  (define (all-ops)
    (in-generator #:arity 3
     (for* ([(symbol op) operators]
            [rank (all-ranks op)])
       (yield symbol (cons (first rank) (second rank)) (third rank)))))

  (define (all-vars)
    vars)

  (define (builtin-term-types)
    builtins)

  (define (merge-signatures other new-sort-graph)
    (define merged-sort-graph
      (or new-sort-graph
          (merge-sort-graphs sort-graph (signature-sort-graph other))))
    (define merged-builtins (set-union builtins (signature-builtins other)))
    (define merged-ops
      (for/fold ([sig (empty-signature merged-sort-graph
                                       #:builtins merged-builtins)])
                ([(symbol rank meta) (stream-append (sequence->stream (all-ops))
                                                    (sequence->stream (send other all-ops)))])
        (unless (andmap sort? (car rank))
          ; Kinds as sort constraints need special attention when merging
          ; signatures, because the merging the sort graph can also merge
          ; formerly distinct kinds, or enlarge existing kinds. Since kinds
          ; are represented as sets of sorts, the original kind representation
          ; is no longer a valid sort.
          (error "kind arguments not yet implemented"))
        (send sig add-op* symbol (car rank) (cdr rank) meta)))
    (define merged-vars
      (for/fold ([sig merged-ops])
                ([(name sort) (stream-append (sequence->stream (all-vars))
                                             (sequence->stream (send other all-vars)))])
        (send sig add-var name sort)))
    merged-vars)

  (define (display-signature indentation port)
    (define prefix (make-string indentation #\space))
    (unless (set-empty? builtins)
      (display " #:builtins " port)
      (print builtins port))
    (display-sort-graph sort-graph indentation port)
    (newline port)
    (display prefix port)
    (display "; operators" port)
    (for ([(symbol rank meta) (all-ops)])
      (newline port)
      (display prefix port)
      (display "(op " port)
      (if (empty? (car rank))
          (display symbol port)
          (display (cons symbol (car rank)) port))
      (display " " port)
      (display (cdr rank) port)
      (display ")" port))
    (newline port)
    (display prefix port)
    (display "; vars" port)
    (for ([(name sort) (all-vars)])
      (newline port)
      (display prefix port)
      (display "(var " port)
      (display name port)
      (display " " port)
      (display sort port)
      (display ")" port)))

  (define (*display* port mode)
    (display "(signature" port)
    (display-signature 2 port)
    (display ")\n" port)))

(define (add-op signature symbol arity sort #:meta [meta #f])
  (add-op* signature symbol arity sort meta))

(define (empty-signature sort-graph #:builtins [builtins (set)])
  (signature sort-graph (optimize-subsort-lookup sort-graph) (hash) (hash) builtins))

(module+ test
  (define a-signature
    (~> (empty-signature sorts)
        (add-op 'foo empty 'B)
        (add-op 'foo (list 'A 'B) 'A)
        (add-var 'a-var 'A)))
  (check-true (regular? a-signature))
  (check-equal? (lookup-op-meta a-signature 'foo empty) (list empty 'B #f))
  (check-equal? (lookup-op a-signature 'foo empty) (cons empty 'B))
  (check-false (lookup-op-meta a-signature 'X empty))
  (check-false (lookup-op a-signature 'X empty))
  (check-equal? (lookup-var a-signature 'a-var) 'A)
  (check-false (lookup-var a-signature 'baz))
  (check-equal? (for/set ([(s r m) (all-ops a-signature)]) r)
                (set (cons empty 'B) (cons (list 'A 'B) 'A)))
  (check-equal? (all-vars a-signature) (hash 'a-var 'A))
  (check-exn exn:fail? (thunk (add-op a-signature 'foo (list 'A 'A) 'X)))
  (check-exn exn:fail? (thunk (add-op a-signature 'a-var 'X)))
  (check-exn exn:fail? (thunk (add-var a-signature 'foo 'X)))
  (check-equal? (merge-signatures (empty-signature sorts)
                                  (empty-signature sorts)
                                  #f)
                (empty-signature sorts))
  (check-equal? (merge-signatures (empty-signature sorts #:builtins (set 'foo))
                                  (empty-signature sorts #:builtins (set 'bar))
                                  #f)
                (empty-signature sorts  #:builtins (set 'foo 'bar)))
  (check-equal? (merge-signatures a-signature (empty-signature sorts) #f)
                a-signature)
  (check-equal? (merge-signatures a-signature (empty-signature sorts) #f)
                a-signature)
  (check-equal? (merge-signatures a-signature a-signature #f)
                a-signature)
  (check-equal? (merge-signatures a-signature
                                  (~> (empty-signature
                                       (~> sorts
                                           (add-subsort-relation 'X 'B)))
                                      (add-op 'bar empty 'X))
                                  #f)
                (~> (empty-signature
                     (~> sorts
                         (add-subsort-relation 'X 'B)))
                    (add-op 'bar empty 'X)
                    (add-op 'foo empty 'B)
                    (add-op 'foo (list 'A 'B) 'A)
                    (add-var 'a-var 'A)))
  (let* ([sorts (~> empty-sort-graph
                    (add-sort 'A) (add-sort 'B) (add-sort 'C)
                    (add-subsort-relation 'A 'B)
                    (add-subsort-relation 'A 'C))]
         [signature (~> (empty-signature sorts)
                        (add-op 'foo (list 'B) 'B)
                        (add-op 'foo (list 'C) 'C))])
    (check-false (regular? signature))
    (check-equal? (non-regular-op-example signature)
                  (list 'foo (list 'A) (list 'B 'C)))
    (check-equal? (non-regular-op-example
                   (add-op signature 'foo (list 'A) 'A))
                  #f)))
