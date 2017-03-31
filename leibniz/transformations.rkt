#lang racket

(provide transform-context-declarations)

(require threading)

(define (transform-context-declarations decls transformations)
  (for/fold ([decls decls])
            ([tr transformations])
    ((apply-transformation tr) decls)))

(define ((apply-transformation tr) decls)
  (match tr
    ['hide-vars
     (~> decls
         (hash-update 'rules (add-context-vars (hash-ref decls 'vars)))
         (hash-set 'vars (set)))]
    [(list 'rename-sort sort1 sort2)
     (rename-sort sort1 sort2 decls)]
    [(list 'add-include cname)
     (~> decls
         (hash-update 'includes (λ (cnames) (append cnames (list cname)))))]))

(define ((add-context-vars var-decls) rule-decls)
  (for/list ([d rule-decls])
    (match d
      [(list 'rule pattern replacement clauses)
       (define local-var-names
         (for/set ([c clauses] #:when (equal? (first c) 'var))
           (second c)))
       (list 'rule pattern replacement
             (append clauses
                     (filter (λ (vd) (not (set-member? local-var-names (second vd))))
                             (set->list var-decls))))])))

(define (rename-sort sort1 sort2 decls)

  (define (new-sort s)
    (if (equal? s sort1) sort2 s))

  (define (rename* x)
    (match x
      [(list 'var name sort)
       (list 'var name (new-sort sort))]
      [(list 'sort sort)
       (list 'sort (new-sort sort))]
      [other other]))

  (~> decls
      (hash-update 'sorts
                   (λ (sorts)
                     (for/set ([s sorts])
                       (new-sort s))))
      (hash-update 'subsorts
                   (λ (subsorts)
                     (for/set ([ss subsorts])
                       (cons (new-sort (car ss)) (new-sort (cdr ss))))))
      (hash-update 'ops
                   (λ (ops)
                     (for/set ([op ops])
                       (match-define (list name arity rsort) op)
                       (list name
                             (map rename* arity)
                             (new-sort rsort)))))
      (hash-update 'vars
                   (λ (vars)
                     (for/set ([v vars])
                       (rename* v))))
      (hash-update 'rules
                   (λ (rules)
                     (for/list ([r rules])
                       (match-define (list 'rule pattern replacement clauses) r)
                       (list 'rule pattern replacement (map rename* clauses)))))
      (hash-update 'equations
                   (λ (eqs)
                     (for/set ([eq eqs])
                       (match-define (list 'equation left right clauses) eq)
                       (list 'equation left right (map rename* clauses)))))
      (hash-set 'locs (hash))))
