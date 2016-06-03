#lang racket

(require "./lightweight-class.rkt"
         "./sorts.rkt"
         rackjure/threading
         racket/generator)

(module+ test
  (require rackunit racket/function rackjure/threading)
  ; Define a simple sort graph for testing
  (define sorts
    (~> (empty-sort-graph)
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
; List of ranks partially sorted by arity
;
(define-class sorted-ranks

  (field sort-graph k-rank ranks)

  (define (check-kinds k-arity k-sort)
    (unless (equal? k-rank (cons k-arity k-sort))
      (error "operator not monotonic"))
    this)

  (define (is-subarity? arity1 arity2)
    (and (= (length arity1) (length arity2))
         (for/and ([s1 arity1] [s2 arity2])
           (is-subsort? sort-graph s1 s2))))

  (define (add-rank arity sort)

    ; Adding a new rank requires
    ; (1) finding the right place to insert it
    ; (2) constructing the new rank list
    ;
    ; (add-rank*) performs (1), keeping a list of already-inspected
    ; ranks (prefix) in reverse order for later use by (insert-rank),
    ; which performs (2).

    (define (insert-rank prefix ranks)
      (sorted-ranks sort-graph k-rank
                    (for/fold ([ranks (cons (cons arity sort) ranks)])
                              ([r prefix])
                      (cons r ranks))))

    (define (add-rank* prefix ranks)
      (if (empty? ranks)
          (insert-rank prefix ranks)
          (match-let* ([(and r (cons a s)) (first ranks)])
            (cond
              [(is-subarity? arity a)
               ; We hit a higher arity, so we must insert the new
               ; one before it. We must also check the sorts
               ; for ensuring monotonicity.
               (if (is-subsort? sort-graph sort s)
                   (insert-rank prefix ranks)
                   (error "operator not monotonic"))
               ]
              [(and (is-subarity? a arity)
                    (not (is-subsort? sort-graph s sort)))
               ; We hit a sub-arity of the one we want to add,
               ; but the corresponding sort is not a sub-sort,
               ; so we signal a monotonicity error.
               (error "operator not monotonic")]
              [else
               ; We get here in two situations:
               ; 1) The new arity is unrelated to the current one.
               ; 2) The new arity is higher than the current one
               ;    AND monotonicity has been verified.
               ; We thus move on to the next entry in the rank list.
               (add-rank* (cons r prefix) (rest ranks))]))))

    (add-rank* empty ranks))

  (define (matching-ranks-for-arity arity)
    (filter (λ (r) (is-subarity? arity (car r))) ranks))

  (define (smallest-rank-for-arity arity)
    (define after (dropf ranks
                         (λ (r) (not (is-subarity? arity (car r))))))
    (if (empty? after)
        k-rank
        (car after)))

  (define (monotonic?)
    ; Monotonicity of the rank lists is guaranteed during construction.
    ; This method is only provided for testing.
    (define (test1 r rs)
      (for/and ([r2 rs])
        (if (is-subarity? (car r) (car r2))
            (is-subsort? sort-graph (cdr r) (cdr r2))
            #t)))
    (define (monotonic* rs n)
      (if (> n 1)
          (and (test1 (first rs) (rest rs))
               (monotonic* (rest rs) (- n 1)))
          #t))
    (monotonic* ranks (length ranks)))

  (define (preregular-rank-list?)
    ; Do a brute-force search over all possible argument sorts
    ; and check that there is always a unique least value sort.
    ; There is a lot of room for improvement.
    (for/and ([arg-sorts (cartesian-product (car k-rank))])
      (define value-sorts (map cdr (matching-ranks-for-arity arg-sorts)))
      (or (< (length value-sorts) 2)
          (for/and ([vs (rest value-sorts)])
            (is-subsort? sort-graph (first value-sorts) vs))))))

(define (empty-sorted-ranks sort-graph k-arity k-sort)
  (define k-rank (cons k-arity k-sort))
  (sorted-ranks sort-graph k-rank empty))

(module+ test

  (define a-sr-1
    (~> (empty-sorted-ranks sorts (list (kind sorts 'A)) (kind sorts 'A))
        (add-rank (list 'A) 'A)
        (add-rank (list 'B) 'B)))

  (define a-sr-2
    (~> (empty-sorted-ranks sorts (list (kind sorts 'X)) (kind sorts 'X))
        (add-rank (list 'Y) 'Y)))

  (define a-sr-3
    (~> (empty-sorted-ranks sorts (list (kind sorts 'A) (kind sorts 'X))
                            (kind sorts 'A))
        (add-rank (list 'A 'X) 'A)
        (add-rank (list 'B 'Y) 'B)))

  (check-true (is-subarity? a-sr-1 (list 'B) (list 'A)))
  (check-true (is-subarity? a-sr-1 (list 'A) (list 'A)))
  (check-true (is-subarity? a-sr-1 (list 'B) (list 'B)))
  (check-false (is-subarity? a-sr-1 (list 'X) (list 'B)))
  (check-false (is-subarity? a-sr-1 (list 'A) (list 'X)))
  (check-false (is-subarity? a-sr-1 (list 'A) (list 'B)))
  (check-false (is-subarity? a-sr-1 (list 'B 'B) (list 'B)))

  (check-equal? (matching-ranks-for-arity a-sr-1 (list 'A))
                (list (cons (list 'A) 'A)))
  (check-equal? (matching-ranks-for-arity a-sr-1 (list 'B))
                (list (cons (list 'B) 'B) (cons (list 'A) 'A)))
  (check-equal? (matching-ranks-for-arity a-sr-2 (list 'Y))
                (list (cons (list 'Y) 'Y)))
  (check-equal? (matching-ranks-for-arity a-sr-2 (list 'X)) empty)

  (check-equal? (smallest-rank-for-arity a-sr-1 (list 'A)) (cons (list 'A) 'A))
  (check-equal? (smallest-rank-for-arity a-sr-1 (list 'B)) (cons (list 'B) 'B))
  (check-equal? (smallest-rank-for-arity a-sr-2 (list 'Y)) (cons (list 'Y) 'Y))
  (check-equal? (smallest-rank-for-arity a-sr-2 (list 'X))
                (cons (list (set 'X 'Y)) (set 'X 'Y)))

  (check-true (monotonic? a-sr-1))
  (check-true (monotonic? a-sr-2))
  (check-true (monotonic? a-sr-3))
  (check-true (preregular-rank-list? a-sr-1))
  (check-true (preregular-rank-list? a-sr-2))
  (check-true (preregular-rank-list? a-sr-3))

  ; Try to make a non-monotonic rank list
  (check-exn exn:fail?
             (thunk (~> (empty-sorted-ranks sorts
                                            (list (kind sorts 'A))
                                            (kind sorts 'A))
                        (add-rank (list 'A) 'B)
                        (add-rank (list 'B) 'A))))

  ; Make a non-preregular rank list, then add a rank to make it preregular
  (let* ([sorts (~> (empty-sort-graph)
                    (add-sort 'A) (add-sort 'B) (add-sort 'C)
                    (add-subsort-relation 'A 'B)
                    (add-subsort-relation 'A 'C))]
         [srl (~> (empty-sorted-ranks sorts
                                      (list (kind sorts 'A))
                                      (kind sorts 'A))
                  (add-rank (list 'B) 'B)
                  (add-rank (list 'C) 'C))])
    (check-false (preregular-rank-list? srl))
    (check-true (preregular-rank-list? (~> srl
                                           (add-rank (list 'A) 'A))))))

;
; Operators
;
(define-class operator

  (field sort-graph ranks-by-k-arity)

  (define (kind-arity arity)
    (map (λ (sc) (kind-constraint sort-graph sc)) arity))

  (define (add arity sort)
    (define k-arity (kind-arity arity))
    (define k-sort (kind-constraint sort-graph sort))
    (define (update ranks)
      (~> ranks
          (check-kinds k-arity k-sort)
          (add-rank arity sort)))
    (operator sort-graph
              (hash-update ranks-by-k-arity k-arity update
                           (thunk (empty-sorted-ranks sort-graph
                                                      k-arity k-sort)))))
  
  (define (lookup arity)
    (define k-arity (kind-arity arity))
    (some~> (hash-ref ranks-by-k-arity k-arity #f)
            (smallest-rank-for-arity arity)))

  (define (preregular-op?)
    (for/and ([srl (hash-values ranks-by-k-arity)])
      (preregular-rank-list? srl))))

(define (empty-operator sort-graph)
  (operator sort-graph (hash)))

(module+ test

  (define an-op
    (~> (empty-operator sorts)
        (add (list 'A) 'A)
        (add (list 'B) 'B)
        (add (list 'Y) 'Y)))

  (check-equal? (dict-count (operator-ranks-by-k-arity an-op)) 2)
  (check-equal? (lookup an-op (list 'A)) (cons (list 'A) 'A))
  (check-equal? (lookup an-op (list 'B)) (cons (list 'B) 'B))
  (check-equal? (lookup an-op (list 'Y)) (cons (list 'Y) 'Y))
  (check-equal? (lookup an-op (list 'X)) (cons (list (set 'X 'Y)) (set 'X 'Y)))

  (check-true (preregular-op? an-op))

  ; Try to make non-monotonic operators
  (check-exn exn:fail?
             (thunk (~> (empty-operator sorts)
                        (add (list 'A) 'B)
                        (add (list 'B) 'A))))
  (check-exn exn:fail?
             (thunk (~> (empty-operator sorts)
                        (add (list 'A) 'B)
                        (add (list 'B) 'X))))

  ; Make a non-preregular operator, then add a rank to make it preregular
  (let* ([sorts (~> (empty-sort-graph)
                    (add-sort 'A) (add-sort 'B) (add-sort 'C)
                    (add-subsort-relation 'A 'B)
                    (add-subsort-relation 'A 'C))]
         [op (~> (empty-operator sorts)
                 (add (list 'B) 'B)
                 (add (list 'C) 'C))])
    (check-false (preregular-op? op))
    (check-true (preregular-op? (~> op (add (list 'A) 'A))))))

;
; Signatures
;
(define-class signature

  (field sort-graph operators)

  (define (add-op symbol arity sort)
    (for ([arg arity])
      (validate-sort-constraint sort-graph arg))
    (validate-sort-constraint sort-graph sort)
    (~>> (add (hash-ref operators symbol
                        (λ () (empty-operator sort-graph)))
              arity sort)
         (hash-set operators symbol)
         (signature sort-graph)))

  (define (lookup-op symbol arity)
    (define op (hash-ref operators symbol #f))
    (and op
         (lookup op arity)))

  (define (preregular?)
    (for/and ([op (hash-values operators)])
      (preregular-op? op))))

(define (empty-signature sort-graph)
  (signature sort-graph (hash)))

(module+ test
  (define a-signature
    (~> (empty-signature sorts)
        (add-op 'foo empty 'B)
        (add-op 'foo (list 'A 'B) 'A)))
  (check-true (preregular? a-signature))
  (check-exn exn:fail? (thunk (add-op signature 'foo (list 'A 'A) 'X))))
