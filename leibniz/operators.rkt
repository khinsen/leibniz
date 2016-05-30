#lang racket

(require "./lightweight-class.rkt"
         "./sorts.rkt"
         rackjure/threading)

(module+ test
  (require rackunit racket/function rackjure/threading)
  (define sorts
    (~> (empty-sort-graph)
        (add-sort 'A) (add-sort 'B)
        (add-subsort-relation 'B 'A)
        (add-sort 'X) (add-sort 'Y)
        (add-subsort-relation 'Y 'X))))

; List of ranks partially sorted by arity

(define-class sorted-ranks

  (field sort-graph k-rank ranks)

  (define (check-rank k-arity k-sort)
    (unless (equal? k-rank (cons k-arity k-sort))
      (error "operator not monotonic"))
    this)

  (define (is-subarity? arity1 arity2)
    (and (= (length arity1) (length arity2))
         (for/and ([s1 arity1] [s2 arity2])
           (is-subsort? sort-graph s1 s2))))

  (define (add-rank arity sort)
    (define-values (before after)
      (splitf-at ranks
                 (λ (r) (not (is-subarity? arity (car r))))))
    (sorted-ranks sort-graph k-rank (append before
                                            (cons (cons arity sort) after))))

  (define (matching-ranks-for-arity arity)
    (filter (λ (r) (is-subarity? arity (car r))) ranks))

  (define (smallest-rank-for-arity arity)
    (define after (dropf ranks
                         (λ (r) (not (is-subarity? arity (car r))))))
    (if (empty? after)
        k-rank
        (car after)))

  (define (monotonic?)
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

  (define (regular?)
    ; TODO
    ; - test for regularity
    #t))

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
    (~> (empty-sorted-ranks sorts (list (kind sorts 'A)) (kind sorts 'A))
        (add-rank (list 'A) 'B)
        (add-rank (list 'B) 'A)))

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
  (check-false (monotonic? a-sr-3)))

; Operators

(define-class operator

  (field sort-graph ranks-by-k-arity)

  (define (kind-arity arity)
    (map (λ (sc) (kind-constraint sort-graph sc)) arity))

  (define (add arity sort)
    (define k-arity (kind-arity arity))
    (define k-sort (kind-constraint sort-graph sort))
    (define (update ranks)
      (~> ranks
          (check-rank k-arity k-sort)
          (add-rank arity sort)))
    (operator sort-graph
              (hash-update ranks-by-k-arity k-arity update
                           (thunk (empty-sorted-ranks sort-graph
                                                      k-arity k-sort)))))
  
  (define (lookup arity)
    (define k-arity (kind-arity arity))
    (some~> (hash-ref ranks-by-k-arity k-arity #f)
            (smallest-rank-for-arity arity))))

(define (empty-operator sort-graph)
  (operator sort-graph (hash)))

(module+ test
  (define an-op
    (~> (empty-operator sorts)
        (add (list 'A) 'A)
        (add (list 'B) 'B)
        (add (list 'Y) 'Y)))
  (check-equal? (lookup an-op (list 'A)) (cons (list 'A) 'A))
  (check-equal? (lookup an-op (list 'B)) (cons (list 'B) 'B))
  (check-equal? (lookup an-op (list 'Y)) (cons (list 'Y) 'Y))
  (check-equal? (lookup an-op (list 'X)) (cons (list (set 'X 'Y)) (set 'X 'Y))))

; Signatures

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
         (lookup op arity))))

(define (empty-signature sort-graph)
  (signature sort-graph (hash)))

(module+ test

  (define a-signature
    (~> (empty-signature sorts)
        (add-op 'foo empty 'B)
        (add-op 'foo (list 'A 'B) 'A)))
  (check-exn exn:fail? (thunk (add-op signature 'foo (list 'A 'A) 'X))))
