#lang racket

(provide
 (contract-out
  [sort-graph?              (any/c . -> . boolean?)]
  [empty-sort-graph         sort-graph?]
  [validate-sort            (sort-graph? symbol? . -> . void?)]
  [add-sort                 (sort-graph? symbol? . -> . sort-graph?)]
  [add-subsort-relation     (sort-graph? symbol? symbol? . -> . sort-graph?)]
  [merge-sort-graphs        (sort-graph? sort-graph? . -> . sort-graph?)]
  [extended-kind            (sort-graph? set? . -> . set?)]
  [all-sorts                (sort-graph? . -> . set?)]
  [all-subsorts             (sort-graph? . -> . set?)]
  [has-sort?                (sort-graph? symbol? . -> . boolean?)]
  [is-subsort?              (sort-graph? symbol? symbol? . -> . boolean?)]
  [kind                     (sort-graph? symbol? . -> . set?)]
  [has-kind?                (sort-graph? set? . -> . boolean?)]
  [maximal-sorts            (sort-graph? set? . -> . set?)]
  [sort?                    (any/c . -> . boolean?)]
  [sort-or-kind?            (any/c . -> . boolean?)]
  [sort-constraint?         (any/c . -> . boolean?)]
  [valid-sort-constraint?   (sort-graph? any/c . -> . boolean?)]
  [validate-sort-constraint (sort-graph? sort-constraint? . -> . void?)]
  [kind-constraint          (sort-graph? sort-constraint?
                                         . -> . sort-constraint?)]
  [conforms-to?             (sort-graph? sort-constraint? sort-constraint?
                                         . -> . boolean?)]
  [constraint->string       (sort-graph? sort-constraint? . -> . string?)]))

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

  (define (is-subsort? sort1 sort2)
    (validate-sort sort1)
    (validate-sort sort2)
    (or (equal? sort1 sort2)
        (let ([ss (hash-ref subsorts sort2)])
          (or (set-member? ss sort1)
              (for/or ([s (in-set ss)])
                (is-subsort? sort1 s))))))
  
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

  (define (merge-sort-graphs s-graph)
    (let ([sg
           (for/fold ([sg this])
                     ([sort (send s-graph all-sorts)])
             (send sg add-sort sort))])
      (for/fold ([sg sg])
                ([ss-relation (send s-graph all-subsorts)])
        (send sg add-subsort-relation (car ss-relation) (cdr ss-relation)))))

  (define (extended-kind sort-set)
    (kind (set-first sort-set)))

  (define (all-sorts)
    (list->set (hash-keys subsorts)))

  (define (all-subsorts)
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
         (has-sort? (set-first x))
         (equal? x (kind (set-first x)))))

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
      [(set? constraint)
       (has-kind? constraint)]
      [else #f]))

  (define (validate-sort-constraint constraint)
    (unless (valid-sort-constraint? constraint)
      (error "invalid sort constraint" constraint)))

  (define (kind-constraint constraint)
    (validate-sort-constraint constraint)
    (cond
      [(equal? #f constraint) #f]
      [(symbol? constraint)   (kind constraint)]
      [else                   constraint]))

  (define (conforms-to? c1 c2)
    (validate-sort-constraint c1)
    (validate-sort-constraint c2)
    (cond
      [(equal? c2 #f)
       #t]
      [(equal? c1 #f)
       #f]
      [(set? c1)
       (equal? c1 c2)]
      [(symbol? c2)
       (is-subsort? c1 c2)]
      [else
       (set-member? c2 c1)]))

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
                                 (set->list (maximal-sorts constraint)))
                            ","))])))

(define empty-sort-graph
  (sort-graph (hash) (hash) (hash)))

;
; Tests for sorts and sort constraints
;
(define (sort? x)
  (symbol? x))

(define (sort-or-kind? x)
  (or (sort? x)
      (and (set? x)
           (not (set-empty? x))
           (for/and ([e x])
             (sort? e)))))

(define (sort-constraint? x)
  (or (equal? x #f)
      (sort-or-kind? x)))

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
  (check-equal? (all-subsorts an-s-graph)
                (set '(A . B) '(A . D) '(B . C)))

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
  (check-equal? (extended-kind merged (kind an-s-graph 'A) )
                (kind merged 'A))
  (check-equal? (extended-kind merged (kind another-s-graph 'A) )
                (kind merged 'A))
  (check-equal? (merge-sort-graphs an-s-graph an-s-graph) an-s-graph)
  (check-equal? (merge-sort-graphs empty-sort-graph an-s-graph) an-s-graph)
  (check-equal? (merge-sort-graphs an-s-graph empty-sort-graph) an-s-graph)
  (check-equal? (kind two-kinds 'A) (kind two-kinds 'C))
  (check-equal? (kind two-kinds 'V) (kind two-kinds 'W))
  (check-not-equal? (kind two-kinds 'A) (kind two-kinds 'W))

  (check-true  (has-kind? two-kinds (kind two-kinds 'A)))
  (check-false (has-kind? two-kinds #f))
  (check-false (has-kind? two-kinds (set)))
  (check-false (has-kind? two-kinds (set 'X)))
  (check-false (has-kind? two-kinds (set 'A)))

  (check-equal? (kind-constraint two-kinds 'A)
                (kind two-kinds 'A))
  (check-equal? (kind-constraint two-kinds (kind two-kinds 'A))
                (kind two-kinds 'A))

  (check-true  (conforms-to? two-kinds 'A 'B))
  (check-true  (conforms-to? two-kinds 'W 'W))
  (check-true  (conforms-to? two-kinds 'A #f))
  (check-true  (conforms-to? two-kinds 'V #f))
  (check-true  (conforms-to? two-kinds 'A (kind two-kinds 'B)))
  (check-true  (conforms-to? two-kinds 'V (kind two-kinds 'W)))
  (check-false (conforms-to? two-kinds 'V (kind two-kinds 'A)))
  (check-false (conforms-to? two-kinds 'A 'V))
  (check-true  (conforms-to? two-kinds (kind two-kinds 'A) (kind two-kinds 'B)))
  (check-false (conforms-to? two-kinds (kind two-kinds 'A) (kind two-kinds 'W)))

  (check-equal? (constraint->string two-kinds 'A) "A")
  (check-equal? (constraint->string two-kinds 'V) "V")
  (check-equal? (constraint->string two-kinds (kind two-kinds 'V)) "[W]")
  (check-equal? (constraint->string two-kinds #f) "[*]"))
