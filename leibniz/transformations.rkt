#lang racket

(provide transform-context-declarations)

(require (prefix-in sorts:  "./sorts.rkt")
         (prefix-in operators:  "./operators.rkt")
         (prefix-in terms:  "./terms.rkt")
         racket/hash
         threading)

;; Test cases are in documents.rkt, to avoid cyclic dependencies.

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
         (hash-update 'assets (λ (assets) (for/hash ([(label value) assets])
                                            (values label (transformer value)))))
         (hash-set 'vars (hash)))]
    [(list 'rename-sort sort1 sort2)
     (replace-sorts decls (λ (s) (if (equal? s sort1) sort2 s)))]
    [(list 'add-include mode cname)
     (add-include decls mode cname)]
    [(list 'real->float fp-sort)
     (real->float decls fp-sort)]))

(define (combine-varsets name sort1 sort2)
  (unless (equal? sort1 sort2)
    (error (format "Var ~a of sort ~a redefined with sort ~a"
                   name sort1 sort2)))
  sort1)

(define ((add-context-vars var-decls) item-decl)
  (define (combined-vars local-vars)
    (hash-union local-vars var-decls #:combine/key combine-varsets))
  (define (transform item)
    (match item
      [(list 'rule vars term1 term2 condition)
       (list 'rule (combined-vars vars) term1 term2 condition)]
      [(list 'equation vars term1 term2 condition)
       (list 'equation (combined-vars vars) term1 term2 condition)]
      [(hash-table (_ _) ...)
       (for/hash ([(label value) item])
         (values label (transform value)))]
      [(list 'term/var name)
       (when (hash-has-key? var-decls name)
         (error (format "variable ~a has been removed" name)))
       item]
      [(list 'term op args)
       (list 'term op (map transform args))]
      [other-term
       other-term]))
  (transform item-decl))

; Replace sorts by applying sort-transformer to every sort in a context

(define (replace-sorts context sort-transformer)

  (define (transform-sorts sorts)
    (for/set ([s sorts])
      (sort-transformer s)))

  (define (transform-subsorts subsorts)
    (for/set ([ss subsorts])
      (cons (sort-transformer (car ss)) (sort-transformer (cdr ss)))))

  (define (transform-vars vars)
    (for/hash ([(name sort) vars])
      (values name (sort-transformer sort))))

  (define (transform-arg x)
    (match x
      [(list 'var name sort)
       (list 'var name (sort-transformer sort))]
      [(list 'sort sort)
       (list 'sort (sort-transformer sort))]))

  (define (transform-ops ops)
    (for/set ([op ops])
      (match-define (list name arity rsort) op)
      (list name
            (map transform-arg arity)
            (sort-transformer rsort))))

  (define (transform-item item)
    (match item
      [(list 'rule vars pattern replacement condition)
       (list 'rule (transform-vars vars) pattern replacement condition)]
      [(list 'equation vars left right condition)
       (list 'equation (transform-vars vars) left right condition)]
      [(hash-table (_ _) ...)
       (transform-assets item)]
      [term
       term]))

  (define (transform-rules rules)
    (map transform-item rules))

  (define (transform-assets assets)
    (for/hash ([(label value) assets])
      (values label (transform-item value))))

  (~> context
      (hash-remove 'locs)
      (hash-update 'sorts transform-sorts)
      (hash-update 'subsorts transform-subsorts)
      (hash-update 'vars transform-vars)
      (hash-update 'ops transform-ops)
      (hash-update 'rules transform-rules)
      (hash-update 'assets transform-assets)))

; Add include (use/extend)

(define (add-include context mode cname)
  (hash-update context 'includes
               (λ (crefs) (append crefs (list (cons mode cname))))))

; Convert exact arithmetic on reals to inexact arithmetic on floats
;
; The principle is to replace all subsorts of ℝ that are not subsorts of ℤ
; by FP32 or FP64. Two additional modifications are required:
;
;  1. Rational constants are converted to float. Integer constants are converted
;     to float if they take the place of a rational value that just happens to be an integer.
;  2. Rewrite rules that replace a float expression by an integer expression
;     are converted by adding an int->float conversion on the replacement term.

(define (real->float context float-sort)

  (define signature (hash-ref context 'compiled-signature))
  (define sort-graph (operators:signature-sort-graph signature))
  (define int-sorts (set-add (sorts:all-subsorts sort-graph 'ℤ) 'ℤ))
  (define non-int-sorts
    (set-subtract (set-add (sorts:all-subsorts sort-graph 'ℝ) 'ℝ) int-sorts))

  (define (transform-sort sort)
    (if (set-member? non-int-sorts sort)
        float-sort
        sort))

  (define (num->float x)
    (case float-sort
      [(FP32) (real->single-flonum x)]
      [(FP64) (real->double-flonum x)]))

  (define (real-sort? sort)
    (sorts:conforms-to? sort-graph sort 'ℝ))

  (define (int-sort? sort)
    (sorts:conforms-to? sort-graph sort 'ℤ))

  (define (transform-literal term expected-sort)
    (if (int-sort? expected-sort)
        term
        (match term
          [(list (or 'integer 'rational 'floating-point) x)
           (list 'floating-point (num->float x))]
          [_ term])))

  (define (transform-term term lvars)
    (match term
      [(list 'term op args)
       (define-values (arg-sorts mod-terms)
         (for/fold ([arg-sorts empty]
                    [mod-terms empty])
                   ([arg (reverse args)])
           (define-values (as mt) (transform-term arg lvars))
           (values (cons as arg-sorts) (cons mt mod-terms))))
       (define rank (operators:lookup-op signature op arg-sorts))
       (unless rank
         (error (format "Illegal term ~a" term)))
       (define mod-args
         (for/list ([rs (car rank)]
                    [mt mod-terms])
           (transform-literal mt rs)))
       (values (cdr rank) (list 'term op mod-args))]
      [(list 'term/var name)
       (values (terms:term.sort (terms:make-var-or-term signature name lvars))
               term)]
      [(list 'integer x)
       (values (terms:term.sort x) term)]
      [(list (and type (or 'rational 'floating-point)) x)
       (values (terms:term.sort x) (list type (num->float x)))]
      [#f
       (values #f #f)]))

  (define (transform-rule rule)
    (match-define (list 'rule vars pattern replacement condition) rule)
    (define-values (psort mod-pattern) (transform-term pattern vars))
    (define-values (rsort mod-replacement) (transform-term replacement vars))
    (define-values (csort mod-condition) (transform-term condition vars))
    (if (or (real-sort? rsort) (real-sort? psort))
        (list 'rule vars
              (transform-literal mod-pattern rsort)
              (transform-literal mod-replacement psort)
              mod-condition)
        (list 'rule vars mod-pattern mod-replacement mod-condition)))

  (define (transform-rules rules)
    (map transform-rule rules))

  (define (transform-equation eq)
    (match-define (list 'equation vars left right condition) eq)
    (define-values (lsort mod-left) (transform-term left vars))
    (define-values (rsort mod-right) (transform-term right vars))
    (define-values (csort mod-condition) (transform-term condition vars))
    (if (or (real-sort? lsort) (real-sort? rsort))
        (list 'equation vars
              (transform-literal mod-left rsort)
              (transform-literal mod-right lsort)
              mod-condition)
        (list 'equation vars mod-left mod-right mod-condition)))

  (define (transform-item item)
    (match item
      [(list 'equation args ...)
       (transform-equation item)]
      [(list 'rule args ...)
       (transform-rule item)]
      [(hash-table (_ _) ...)
       (for/hash ([(label value) item])
         (values label (transform-item value)))]
      [term
       (define-values (t-sort t-term) (transform-term term (hash)))
       t-term]))

  (define (transform-assets assets)
    (for/hash ([(label value) assets])
      (values label (transform-item value))))

  (~> context
      (hash-update 'rules transform-rules)
      (hash-update 'assets transform-assets)
      (replace-sorts transform-sort)
      (add-include 'use "builtins/IEEE-floating-point")))
