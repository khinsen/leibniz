#lang racket

(provide empty-sort-graph sort-graph?
         add-sort add-subsort merge-with
         all-sorts all-subsorts
         has-sort? is-subsort?
         kind has-kind? maximal-sorts
         valid-sort-constraint? conforms-to?
         constraint->string)

(require "./lightweight-class.rkt"
         rackjure/threading)

(module+ test
  (require rackunit racket/function))

(define-class sort-graph

  (field kinds supersorts subsorts)
  
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

  (define (add-subsort subsort supersort)
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

  (define (merge-with s-graph)
    (let ([sg
           (for/fold ([sg this])
                     ([sort (send s-graph all-sorts)])
             (send sg add-sort sort))])
      (for/fold ([sg sg])
                ([ss-relation (send s-graph all-subsorts)])
        (send sg add-subsort (car ss-relation) (cdr ss-relation)))))

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

(define (empty-sort-graph)
  (sort-graph (hash) (hash) (hash)))

(module+ test
  (define an-s-graph
    (~> (empty-sort-graph)
        (add-sort 'A) (add-sort 'B)
        (add-sort 'C) (add-sort 'D)
        (add-subsort 'A 'B) (add-subsort 'B 'C)
        (add-subsort 'A 'D)))
  (define another-s-graph
    (~> (empty-sort-graph)
        (add-sort 'A)
        (add-sort 'X) (add-sort 'Y)
        (add-subsort 'A 'X)
        (add-subsort 'X 'Y)))
  (define merged (merge-with an-s-graph another-s-graph))
  (define two-kinds
    (~> an-s-graph
        (add-sort 'V) (add-sort 'W)
        (add-subsort 'V 'W)))

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
  (check-exn exn:fail? (thunk (add-subsort an-s-graph 'A 'A)))
  (check-exn exn:fail? (thunk (add-subsort an-s-graph 'A 'X)))
  (check-exn exn:fail? (thunk (add-subsort an-s-graph 'X 'A)))
  (check-exn exn:fail? (thunk (add-subsort an-s-graph 'C 'A)))

  (check-equal? merged
                (merge-with another-s-graph an-s-graph))
  (check-true (has-sort? merged 'A))
  (check-true (has-sort? merged 'X))
  (check-true (is-subsort? merged 'A 'X ))
  (check-true (is-subsort? merged 'A 'C ))

  (check-equal? (kind two-kinds 'A) (kind two-kinds 'C))
  (check-equal? (kind two-kinds 'V) (kind two-kinds 'W))
  (check-not-equal? (kind two-kinds 'A) (kind two-kinds 'W))

  (check-true  (has-kind? two-kinds (kind two-kinds 'A)))
  (check-false (has-kind? two-kinds #f))
  (check-false (has-kind? two-kinds (set)))
  (check-false (has-kind? two-kinds (set 'X)))
  (check-false (has-kind? two-kinds (set 'A)))

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
