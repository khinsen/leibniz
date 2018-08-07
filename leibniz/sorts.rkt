#lang racket

(provide
 (rename-out [is-sort-graph? sort-graph?])
 (contract-out
  [empty-sort-graph         sort-graph?]
  [validate-sort            (sort-graph? symbol? . -> . void?)]
  [add-sort                 (sort-graph? symbol? . -> . sort-graph?)]
  [add-subsort-relation     (sort-graph? symbol? symbol? . -> . sort-graph?)]
  [merge-sort-graphs        (sort-graph? sort-graph? . -> . sort-graph?)]
  [optimize-subsort-lookup  (sort-graph? . -> . sort-graph?)]
  [all-sorts                (sort-graph? . -> . set?)]
  [all-subsort-relations    (sort-graph? . -> . set?)]
  [all-subsorts             (sort-graph? sort? . -> . set?)]
  [has-multiple-supersorts? (sort-graph? sort? . -> . boolean?)]
  [has-sort?                (sort-graph? symbol? . -> . boolean?)]
  [is-subsort?              (sort-graph? symbol? symbol? . -> . boolean?)]
  [kind                     (sort-graph? sort-or-kind? . -> . set?)]
  [has-kind?                (sort-graph? set? . -> . boolean?)]
  [maximal-sorts            (sort-graph? set? . -> . set?)]
  [sort?                    (any/c . -> . boolean?)]
  [kind?                    (any/c . -> . boolean?)]
  [sort-or-kind?            (any/c . -> . boolean?)]
  [sort-constraint?         (any/c . -> . boolean?)]
  [valid-sort-constraint?   (sort-graph? any/c . -> . boolean?)]
  [validate-sort-constraint (sort-graph? sort-constraint? . -> . void?)]
  [kind-constraint          (sort-graph? sort-constraint?
                                         . -> . sort-constraint?)]
  [conforms-to?             (sort-graph? sort-constraint? sort-constraint?
                                         . -> . boolean?)]
  [conforming-sorts         (sort-graph? sort-constraint? . -> . set?)]
  [constraint->string       (sort-graph? sort-constraint? . -> . string?)]
  [connected-components     (sort-graph? . -> . (listof sort-graph?))]
  [display-sort-graph       (sort-graph? natural-number/c output-port?
                                         . -> . void?)]))

(require "./lightweight-class.rkt")

(module+ test
  (require rackunit racket/function threading))

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
;  - a kind allows any sort that belongs to it
;

; Sort graphs
;
(define-class sort-graph

  (field kinds supersorts subsorts subsort-cache)
  ; kinds: a hash mapping sorts to their kinds, each kind being a set of sorts
  ; supersorts: a hash mapping sorts to their immediate supersorts
  ; subsorts: a hash mapping sorts to their immediate subsorts
  ; subsort-cache: #f or a hash mapping sorts tp a set of all their
  ;                subsorts, immediate and indirect.
  ;
  ; The information in supersorts and subsorts is identical (a
  ; directed graph); each hash can be created from the other. The set
  ; of kinds can also be computed from the subsort relations. All
  ; three are stored explicitly for more efficient lookup.

  #:write-proc *display*

  (define (has-sort? sort)
    (hash-has-key? subsorts sort))

  (define (is-subsort? sort1 sort2)
    (cond
      [subsort-cache
       (set-member? (hash-ref subsort-cache sort2) sort1)]
      [else
       (validate-sort sort1)
       (validate-sort sort2)
       (or (equal? sort1 sort2)
           (let ([ss (hash-ref subsorts sort2)])
             (or (set-member? ss sort1)
                 (for/or ([s (in-set ss)])
                   (is-subsort? sort1 s)))))]))
  
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
                    (hash-set subsorts new-sort (set))
                    #f)))

  (define (add-subsort-relation subsort supersort)
    (validate-sort subsort)
    (validate-sort supersort)
    (if (equal? subsort supersort)
        this ; don't record a sort as its own subsort
        (begin
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
                                   (λ (s) (set-add s subsort)))
                      #f))))

  (define (merge-sort-graphs s-graph)
    (let ([sg
           (for/fold ([sg this])
                     ([sort (send s-graph all-sorts)])
             (send sg add-sort sort))])
      (for/fold ([sg sg])
                ([ss-relation (send s-graph all-subsort-relations)])
        (send sg add-subsort-relation (car ss-relation) (cdr ss-relation)))))

  (define (optimize-subsort-lookup)
    (define (full-subsort-set acc sort)
      (cond
        [(hash-has-key? acc sort) acc]
        [else
         (define i-ss (hash-ref subsorts sort))
         (for/fold ([acc (hash-set acc sort (set-add i-ss sort))])
                   ([s (in-set i-ss)])
          (define new-acc (full-subsort-set acc s))
           (hash-update new-acc sort
                        (λ (ss) (set-union ss (hash-ref new-acc s)))))]))
    (cond
      [subsort-cache
       this]
      [else
       (define cache
         (for/fold ([acc (hash)])
                   ([sort (hash-keys subsorts)])
           (full-subsort-set acc sort)))
       (sort-graph kinds supersorts subsorts cache)]))

  (define (all-sorts)
    (list->set (hash-keys subsorts)))

  (define (all-subsort-relations)
    (list->set
     (apply append
            (hash-map supersorts
                      (λ (s1 ss) (for/list ([s2 (in-set ss)])
                                   (cons s1 s2)))))))

  (define (all-subsorts sort)
    (cond
      [subsort-cache
       (hash-ref subsort-cache sort)]
      [else
       (define ss (hash-ref subsorts sort))
       (for/fold ([ss ss])
                 ([s ss])
         (set-union ss (all-subsorts s)))]))

  (define (has-multiple-supersorts? sort)
    (> (set-count (hash-ref supersorts sort (set))) 1))

  (define (kind sort-or-kind)
    (if (sort? sort-or-kind)
        (begin
          (validate-sort sort-or-kind)
          (hash-ref kinds sort-or-kind))
        sort-or-kind))

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
       (equal? c1 (conforming-sorts c2))]
      [(symbol? c2)
       (is-subsort? c1 c2)]
      [else
       (set-member? c2 c1)]))

  (define (conforming-sorts constraint)
    (cond
      [(equal? constraint #f)
       (all-sorts)]
      [(symbol? constraint)
       (set-add (all-subsorts constraint) constraint)]
      [else ; kind
       constraint]))

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
                            ","))]))

  (define (connected-components)
    (define components (list->set (hash-values kinds)))
    (if (equal? (set-count components) 1)
        (list this)
        (for/list ([k components])
          (sort-graph (for/hash ([s k]) (values s k))
                      (for/hash ([s k]) (values s (hash-ref supersorts s)))
                      (for/hash ([s k]) (values s (hash-ref subsorts s)))
                      #f))))

  (define (display-sort-graph indentation port)
    (define prefix (make-string indentation #\space))
    (for ([cc (connected-components)])
      (define k (send cc all-sorts))
      (newline port)
      (display prefix port)
      (display "; kind " port)
      (display (constraint->string k) port)
      (newline port)
      (display prefix port)
      (display "(sorts" port)
      (for ([sort (in-set k)])
        (display #\space port)
        (display sort port)) 
      (display ")\n" port)
      (display prefix port)
      (display "(subsorts" port)
      (for ([ss (in-set (send cc all-subsort-relations))])
        (display " [" port)
        (display (car ss) port)
        (display " " port)
        (display (cdr ss) port)
        (display "]" port)) 
      (display ")" port)))

  (define (*display* port mode)
    (display "(sort-graph" port)
    (display-sort-graph 2 port)
    (display ")\n" port)))

(define empty-sort-graph
  (sort-graph (hash) (hash) (hash) #f))

(define (is-sort-graph? x)
  (sort-graph? x))

;
; Tests for sorts and sort constraints
;
(define (sort? x)
  (symbol? x))

(define (kind? x)
  (and (set? x)
       (not (set-empty? x))
       (for/and ([e x])
         (sort? e))))

(define (sort-or-kind? x)
  (or (sort? x)
      (kind? x)))

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
  (check-equal? (all-subsort-relations an-s-graph)
                (set '(A . B) '(A . D) '(B . C)))

  (check-equal? (all-subsorts an-s-graph 'B) (set 'A))
  (check-equal? (all-subsorts an-s-graph 'C) (set 'A 'B))
  (check-equal? (all-subsorts merged 'Y) (set 'A 'X))
  (check-equal? (all-subsorts two-kinds 'W) (set 'V))

  (check-exn exn:fail? (thunk (add-sort an-s-graph #t)))
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

  (check-equal? (conforming-sorts two-kinds 'B)
                (set 'A 'B))
  (check-equal? (conforming-sorts two-kinds (kind two-kinds 'B))
                (set 'A 'B 'C 'D))
  (check-equal? (conforming-sorts two-kinds #f)
                (set 'A 'B 'C 'D 'V 'W))

  (check-equal? (constraint->string two-kinds 'A) "A")
  (check-equal? (constraint->string two-kinds 'V) "V")
  (check-equal? (constraint->string two-kinds (kind two-kinds 'V)) "[W]")
  (check-equal? (constraint->string two-kinds #f) "[*]")

  (check-equal? (connected-components an-s-graph)
                (list an-s-graph))
  (check-equal? (connected-components another-s-graph)
                (list another-s-graph))
  (check-equal? (connected-components merged)
                (list merged))
  (check-equal? (list->set (connected-components two-kinds))
                (set an-s-graph
                     (~> empty-sort-graph
                         (add-sort 'V) (add-sort 'W)
                         (add-subsort-relation 'V 'W))))

  (check-equal? (~> an-s-graph
                    (optimize-subsort-lookup)
                    (sort-graph-subsort-cache))
                (hash 'A (set 'A)
                      'B (set 'A 'B)
                      'C (set 'A 'B 'C)
                      'D (set 'A 'D)))
  (check-equal? (~> merged
                    (optimize-subsort-lookup)
                    (sort-graph-subsort-cache))
                (hash 'A (set 'A)
                      'B (set 'A 'B)
                      'C (set 'A 'B 'C)
                      'D (set 'A 'D)
                      'X (set 'A 'X)
                      'Y (set 'A 'X 'Y)))
  (check-equal? (~> two-kinds
                    (optimize-subsort-lookup)
                    (sort-graph-subsort-cache))
                (hash 'A (set 'A)
                      'B (set 'A 'B)
                      'C (set 'A 'B 'C)
                      'D (set 'A 'D)
                      'V (set 'V)
                      'W (set 'V 'W))))
