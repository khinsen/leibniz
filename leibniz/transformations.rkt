#lang racket

(provide transform-context-declarations)

(require racket/hash
         threading)

(define (transform-context-declarations decls transformations)
  (for/fold ([decls decls])
            ([tr transformations])
    ((apply-transformation tr) decls)))

(define ((apply-transformation tr) decls)
  (match tr
    ['hide-vars
     (define transformer (add-context-vars (hash-ref decls 'vars)))
     (~> decls
         (hash-update 'rules (λ (rules) (for/list ([r rules])
                                          (transformer r))))
         (hash-update 'equations (λ (eqs) (for/hash ([(label eq) eqs])
                                            (values label (transformer eq)))))
         (hash-set 'vars (hash)))]
    [(list 'rename-sort sort1 sort2)
     (rename-sort sort1 sort2 decls)]
    [(list 'add-include mode cname)
     (~> decls
         (hash-update 'includes (λ (cnames) (append cnames (list (cons mode cname))))))]))

(define (combine-varsets name sort1 sort2)
  (unless (equal? sort1 sort2)
    (error (format "Var ~a of sort ~a redefined with sort ~a"
                   name sort1 sort2)))
  sort1)

(define ((add-context-vars var-decls) rule-or-eq-decl)
  (match rule-or-eq-decl
    [(list 'rule vars term1 term2 condition)
     (define combined-vars 
       (hash-union vars var-decls #:combine/key combine-varsets))
     (list 'rule combined-vars term1 term2 condition)]
    [(list 'equation label vars term1 term2 condition)
     (define combined-vars 
       (hash-union vars var-decls #:combine/key combine-varsets))
     (list 'equation label combined-vars term1 term2 condition)]))

(define (rename-sort sort1 sort2 decls)

  (define (new-sort s)
    (if (equal? s sort1) sort2 s))

  (define (rename-arg x)
    (match x
      [(list 'var name sort)
       (list 'var name (new-sort sort))]
      [(list 'sort sort)
       (list 'sort (new-sort sort))]
      [other other]))

  (define (rename-vars vars)
    (for/hash ([(name sort) vars])
      (values name (new-sort sort))))

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
                             (map rename-arg arity)
                             (new-sort rsort)))))
      (hash-update 'vars rename-vars)
      (hash-update 'rules
                   (λ (rules)
                     (for/list ([r rules])
                       (match-define (list 'rule vars pattern replacement condition) r)
                       (list 'rule (rename-vars vars) pattern replacement condition))))
      (hash-update 'equations
                   (λ (eqs)
                     (for/hash ([(label  eq) eqs])
                       (match-define (list 'equation label2 vars left right condition) eq)
                       (unless (equal? label label2)
                         (error "can't happen"))
                       (values label
                               (list 'equation label2 (rename-vars vars)
                                     left right condition)))))
      (hash-set 'locs (hash))))
