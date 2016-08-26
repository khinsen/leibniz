#lang racket

(provide
 (contract-out
  [sort-graph?              (any/c . -> . boolean?)]
  [kind-of?                 (any/c . -> . boolean?)]
  [empty-sort-graph         sort-graph?]
  [validate-sort            (sort-graph? symbol? . -> . void?)]
  [add-sort                 (sort-graph? symbol? . -> . sort-graph?)]
  [add-subsort-relation     (sort-graph? symbol? symbol? . -> . sort-graph?)]
  [merge-sort-graphs        (sort-graph? sort-graph? . -> . sort-graph?)]
  [all-sorts                (sort-graph? . -> . set?)]
  [all-subsort-relations    (sort-graph? . -> . set?)]
  [all-subsorts             (sort-graph? sort? . -> . set?)]
  [has-sort?                (sort-graph? symbol? . -> . boolean?)]
  [is-subsort?              (sort-graph? symbol? symbol? . -> . boolean?)]
  [kind                     (sort-graph? symbol? . -> . kind?)]
  [has-kind?                (sort-graph? kind? . -> . boolean?)]
  [valid-sort-constraint?   (sort-graph? any/c . -> . boolean?)]
  [validate-sort-constraint (sort-graph? sort-constraint? . -> . void?)]
  [kind-constraint          (sort-graph? sort-constraint?
                                         . -> . sort-constraint?)]
  [conforms-to?             (sort-graph? sort-constraint? sort-constraint?
                                         . -> . boolean?)]
  [conforming-sorts         (sort-graph? sort-constraint? . -> . set?)]
  [equivalent-constraints?  (sort-graph? sort-constraint? sort-constraint?
                                         . -> . boolean?)]
  [constraint->string       (sort-graph? sort-constraint? . -> . string?)]
  [sort?                    (any/c . -> . boolean?)]
  [sort-or-kind?            (any/c . -> . boolean?)]
  [sort-constraint?         (any/c . -> . boolean?)]))

(require "./lightweight-class.rkt")

(module+ test
  (require rackunit racket/function rackjure/threading))

;
; Sorts and kinds
;
; A sort is nothing but a label, represented by a Racket symbol.  A
; sort can be declared a subsort of other sorts. The subsort relations
; define a partial order on sorts and form a directed acyclic graph
; (DAG). Each connected component of this graph defines a kind. Sorts
; that belong to different kinds are completely unrelated. Kinds
; matter because operator definitions are subject to constraints on
; their argument sorts if there are multiple definitions for sorts in
; the same kind. Kinds are represented as sets of the sorts they contain,
; i.e. as sets of symbols.
;
; Sort and kind constraints are used in operator and variable definitions.
; They limit the sorts which can be used in certain places. There are three
; types of sort constraints:
;  - #f stands for no constraint
;  - a sort allows itself and any of its subsorts
;  - a kind constraint, represented by a value of structure kind-of
;    that contains a sort symbol, stands for "all sorts that are of the
;    same kind as this sort".
;

(struct kind-of (sort) #:transparent)

;
; Sort graphs
;
(define-class sort-graph

  (field kinds supersorts subsorts)
  ; kinds: a hash mapping sorts to their kinds, each kind being a set of sorts
  ; supersorts: a hash mapping sorts to their immediate supersorts
  ; subsorts: a hash mapping sorts to their immediate subsorts

  ; The information in supersorts and subsorts is identical (a
  ; directed graph); each hash can be created from the other. The set
  ; of kinds can also be computed from the subsort relations. All
  ; three are stored explicitly for more efficient lookup.

  (define (has-sort? sort)
    (hash-has-key? subsorts sort))

  (define (validate-sort sort)
    (unless (has-sort? sort)
      (error "undefined sort" sort)))
  
  (define (add-sort new-sort)
    (unless (symbol? new-sort)
      (error "not a valid sort:" new-sort))
    (if (has-sort? new-sort)
        this
        (sort-graph (hash-set kinds new-sort (set new-sort))
                    (hash-set supersorts new-sort (set))
                    (hash-set subsorts new-sort (set)))))

  (define (add-subsort-relation subsort supersort)
    (validate-sort subsort)
    (validate-sort supersort)
    (when (equal? subsort supersort)
      (error "sorts are equal:" subsort supersort))
    (when (is-subsort? supersort subsort)
      (error "cycle in subsort relation:" supersort subsort))
    (sort-graph (let ([new-kind (set-union (hash-ref kinds supersort)
                                           (hash-ref kinds subsort))])
                  (for/fold ([kinds kinds])
                            ([sort (in-set new-kind)])
                    (hash-set kinds sort new-kind)))
                (hash-update supersorts subsort
                             (λ (s) (set-add s supersort)))
                (hash-update subsorts supersort
                             (λ (s) (set-add s subsort)))))

  (define (is-subsort? sort1 sort2)
    (validate-sort sort1)
    (validate-sort sort2)
    (or (equal? sort1 sort2)
        (let ([ss (hash-ref subsorts sort2)])
          (or (set-member? ss sort1)
              (for/or ([s (in-set ss)])
                (is-subsort? sort1 s))))))

  (define (all-subsorts sort)
    (define ss (hash-ref subsorts sort))
    (for/fold ([ss ss])
              ([s ss])
      (set-union ss (all-subsorts s))))

  (define (merge-sort-graphs s-graph)
    (let ([sg
           (for/fold ([sg this])
                     ([sort (send s-graph all-sorts)])
             (send sg add-sort sort))])
      (for/fold ([sg sg])
                ([ss-relation (send s-graph all-subsort-relations)])
        (send sg add-subsort-relation (car ss-relation) (cdr ss-relation)))))

  (define (all-sorts)
    (list->set (hash-keys subsorts)))

  (define (all-subsort-relations)
    (list->set
     (apply append
            (hash-map supersorts
                      (λ (s1 ss) (for/list ([s2 (in-set ss)])
                                   (cons s1 s2)))))))

  (define (kind sort)
    (validate-sort sort)
    (hash-ref kinds sort))

  (define (has-kind? x)
    (and (set? x)
         (not (set-empty? x))
         (equal? x (hash-ref kinds (set-first x) #f))))

  (define (maximal-sorts kind)
    (for/set ([s (in-set kind)]
              #:when (set-empty? (hash-ref supersorts s)))
      s))
  
  (define (valid-sort-constraint? constraint)
    (cond
      [(equal? #f constraint)
       #t]
      [(symbol? constraint)
       (has-sort? constraint)]
      [(kind-of? constraint)
       (has-sort? (kind-of-sort constraint))]
      [else #f]))

  (define (validate-sort-constraint constraint)
    (unless (valid-sort-constraint? constraint)
      (error "invalid sort constraint" constraint)))

  (define (kind-constraint constraint)
    (validate-sort-constraint constraint)
    (cond
      [(equal? #f constraint) #f]
      [(symbol? constraint)   (kind-of constraint)]
      [else                   constraint]))

  (define (conforms-to? c1 c2)
    ; Checks if sort constraint c1 is narrower than sort constraint c2.
    (validate-sort-constraint c1)
    (validate-sort-constraint c2)
    (cond
      [(equal? c2 #f)
       #t]
      [(equal? c1 #f)
       #f]
      [(kind-of? c1)
       (and (kind-of? c2)
            (set-member? (kind (kind-of-sort c1)) (kind-of-sort c2)))]
      [(symbol? c2)
       (is-subsort? c1 c2)]
      [else  ; remaining case: (and (symbol? c1) (kind-of? c2))
       (set-member? (kind (kind-of-sort c2)) c1)]))

  (define (conforming-sorts constraint)
    (cond
      [(equal? constraint #f)
       (all-sorts)]
      [(symbol? constraint)
       (set-add (all-subsorts constraint) constraint)]
      [else ; kind-of
       (kind (kind-of-sort constraint))]))

  (define (equivalent-constraints? c1 c2)
    (validate-sort-constraint c1)
    (validate-sort-constraint c2)
    (cond
      [(kind-of? c1)
       (and (kind-of? c2)
            (set-member? (kind (kind-of-sort c1)) (kind-of-sort c2)))]
      [else
       (equal? c1 c2)]))

  (define (constraint->string constraint)
    (validate-sort-constraint constraint)
    (cond
      [(equal? constraint #f)
       "[*]"]
      [(symbol? constraint)
       (symbol->string constraint)]
      [else
       (format "[~a]"
               (string-join (map symbol->string
                                 (set->list (maximal-sorts (kind (kind-of-sort constraint)))))
                            ","))])))

(define empty-sort-graph
  (sort-graph (hash) (hash) (hash)))

;
; Tests for sorts and sort constraints
;
(define (sort? x)
  (symbol? x))

(define (kind? x)
  (set? x))

(define (sort-or-kind? x)
  (or (sort? x)
      (kind? x)))

(define (sort-constraint? x)
  (or (equal? x #f)
      (sort? x)
      (kind-of? x)))

;
; Unit tests
;
(module+ test
  (define an-s-graph
    (~> empty-sort-graph
        (add-sort 'A) (add-sort 'B)
        (add-sort 'C) (add-sort 'D)
        (add-subsort-relation 'A 'B) (add-subsort-relation 'B 'C)
        (add-subsort-relation 'A 'D)))
  (define another-s-graph
    (~> empty-sort-graph
        (add-sort 'A)
        (add-sort 'X) (add-sort 'Y)
        (add-subsort-relation 'A 'X)
        (add-subsort-relation 'X 'Y)))
  (define merged (merge-sort-graphs an-s-graph another-s-graph))
  (define two-kinds
    (~> an-s-graph
        (add-sort 'V) (add-sort 'W)
        (add-subsort-relation 'V 'W)))

  (check-equal? an-s-graph (add-sort an-s-graph 'A))

  (check-true (has-sort? an-s-graph 'A))
  (check-true (has-sort? an-s-graph 'B))
  (check-true (has-sort? an-s-graph 'C))
  (check-true (has-sort? an-s-graph 'D))
  (check-false (has-sort? an-s-graph 'X))
  (check-equal? (all-sorts an-s-graph)
                (set 'A 'B 'C 'D))

  (check-true (is-subsort? an-s-graph 'A 'A))
  (check-true (is-subsort? an-s-graph 'A 'B))
  (check-true (is-subsort? an-s-graph 'B 'C))
  (check-true (is-subsort? an-s-graph 'A 'C))
  (check-true (is-subsort? an-s-graph 'A 'D))
  (check-false (is-subsort? an-s-graph 'C 'A))
  (check-equal? (all-subsort-relations an-s-graph)
                (set '(A . B) '(A . D) '(B . C)))

  (check-equal? (all-subsorts an-s-graph 'B) (set 'A))
  (check-equal? (all-subsorts an-s-graph 'C) (set 'A 'B))
  (check-equal? (all-subsorts merged 'Y) (set 'A 'X))
  (check-equal? (all-subsorts two-kinds 'W) (set 'V))

  (check-exn exn:fail? (thunk (add-sort an-s-graph #t)))
  (check-exn exn:fail? (thunk (add-subsort-relation an-s-graph 'A 'A)))
  (check-exn exn:fail? (thunk (add-subsort-relation an-s-graph 'A 'X)))
  (check-exn exn:fail? (thunk (add-subsort-relation an-s-graph 'X 'A)))
  (check-exn exn:fail? (thunk (add-subsort-relation an-s-graph 'C 'A)))

  (check-equal? merged
                (merge-sort-graphs another-s-graph an-s-graph))
  (check-true (has-sort? merged 'A))
  (check-true (has-sort? merged 'X))
  (check-true (is-subsort? merged 'A 'X ))
  (check-true (is-subsort? merged 'A 'C ))
  (check-equal? (merge-sort-graphs an-s-graph an-s-graph) an-s-graph)
  (check-equal? (merge-sort-graphs empty-sort-graph an-s-graph) an-s-graph)
  (check-equal? (merge-sort-graphs an-s-graph empty-sort-graph) an-s-graph)
  (check-equal? (kind two-kinds 'A) (kind two-kinds 'C))
  (check-equal? (kind two-kinds 'V) (kind two-kinds 'W))
  (check-not-equal? (kind two-kinds 'A) (kind two-kinds 'W))

  (check-true  (has-kind? two-kinds (kind two-kinds 'A)))
  (check-false (has-kind? two-kinds #f))
  (check-false (has-kind? two-kinds (set 'A)))

  (check-equal? (kind-constraint two-kinds 'A)
                (kind-of 'A))
  (check-equal? (kind-constraint two-kinds (kind-of 'A))
                (kind-of 'A))

  (check-true  (conforms-to? two-kinds 'A 'B))
  (check-true  (conforms-to? two-kinds 'W 'W))
  (check-true  (conforms-to? two-kinds 'A #f))
  (check-true  (conforms-to? two-kinds 'V #f))
  (check-true  (conforms-to? two-kinds 'A (kind-of 'B)))
  (check-true  (conforms-to? two-kinds 'V (kind-of 'W)))
  (check-false (conforms-to? two-kinds 'V (kind-of 'A)))
  (check-false (conforms-to? two-kinds 'A 'V))
  (check-true  (conforms-to? two-kinds (kind-of 'A) (kind-of 'B)))
  (check-false (conforms-to? two-kinds (kind-of 'A) (kind-of 'W)))

  (check-equal? (conforming-sorts two-kinds 'B)
                (set 'A 'B))
  (check-equal? (conforming-sorts two-kinds (kind-of 'B))
                (set 'A 'B 'C 'D))
  (check-equal? (conforming-sorts two-kinds #f)
                (set 'A 'B 'C 'D 'V 'W))

  (check-true (equivalent-constraints? two-kinds (kind-of 'A) (kind-of 'A)))
  (check-true (equivalent-constraints? two-kinds (kind-of 'A) (kind-of 'B)))
  (check-true (equivalent-constraints? two-kinds (kind-of 'V) (kind-of 'W)))
  (check-false (equivalent-constraints? two-kinds (kind-of 'A) (kind-of 'W)))

  (check-equal? (constraint->string two-kinds 'A) "A")
  (check-equal? (constraint->string two-kinds 'V) "V")
  (check-equal? (constraint->string two-kinds (kind-of 'V)) "[W]")
  (check-equal? (constraint->string two-kinds #f) "[*]"))
