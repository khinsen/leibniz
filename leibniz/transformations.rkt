#lang racket

(provide transform-context-declarations)

(require threading)

(define (tr x)
  (displayln x)
  x)

(define (transform-context-declarations decls transformations)
  (for/fold ([decls decls])
            ([tr transformations])
    ((apply-transformation tr) decls)))

(define ((apply-transformation tr) decls)
  (match tr
    ['hide-vars
     (~> decls
         (hash-update 'rules (add-context-vars (hash-ref decls 'vars)))
         (hash-set 'vars empty))]
    [(list 'rename-sort sort1 sort2)
     (for/hash ([(key value) (in-hash decls)])
       (values key (rename-sort sort1 sort2 value)))]))

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
                             var-decls)))])))

(define (rename-sort sort1 sort2 decls)

  (define (new-sort s)
    (if (equal? s sort1) sort2 s))

  (for/list ([decl decls])
    (match decl
      [(list 'sort s)
       (list 'sort (new-sort s))]
      [(list 'subsort s1 s2)
       (list 'subsort (new-sort s1) (new-sort s2))]
      [(list 'op name args rsort)
       (list 'op name (rename-sort sort1 sort2 args) (new-sort rsort))]
      [(list 'var name s)
       (list 'var  name (new-sort s))]
      [(list 'rule pattern replacement clauses)
       (list 'rule pattern replacement (rename-sort sort1 sort2 clauses))]
      [decl decl])))
