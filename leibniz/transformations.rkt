#lang racket

(provide transform-context-declarations
         hide-context-vars)

(require (prefix-in sorts:  "./sorts.rkt")
         (prefix-in operators:  "./operators.rkt")
         (prefix-in terms:  "./terms.rkt")
         racket/hash
         threading)

;; Test cases are in documents.rkt, to avoid cyclic dependencies.

;; Hide context-level variables by pushing them into rule and equation definitions.

(define (hide-context-vars context)
  (define transformer (add-context-vars (hash-ref context 'vars)))
  (~> context
      (hash-update 'rules (λ (rules) (for/list ([r rules])
                                       (transformer r))))
      (hash-update 'assets (λ (assets) (for/hash ([(label value) assets])
                                         (values label (transformer value)))))
      (hash-set 'vars (hash))))

(define ((add-context-vars var-decls) item-decl)
  (define (combined-vars local-vars)
    ;; The context var is added only if no local var of the same name
    ;; already exists.
    (hash-union local-vars var-decls #:combine (λ (a b) a)))
  (define (transform item)
    (match item
      [(list (and type-tag (or 'equation 'rule 'transformation))
             vars term1 term2 condition)
       (list type-tag (combined-vars vars) term1 term2 condition)]
      [(list 'assets assets)
       (list 'assets
             (for/hash ([(label value) assets])
               (values label (transform value))))]
      [(or (list 'as-equation _)
           (list 'as-rule _ _)
           (list 'substitute _ _)
           (list 'transform _ _))
       item]
      [(list 'term/var name)
       (when (hash-has-key? var-decls name)
         (error (format "variable ~a has been removed" name)))
       item]
      [(list 'term op args)
       (list 'term op (map transform args))]
      [other-term
       other-term]))
  (transform item-decl))

;; Apply transformations to a context

(define (transform-context-declarations context transformations)

  (define (transform-included-contexts crefs)
    (for/list ([cref crefs])
      (match-define (cons mode name-or-context) cref)
      (if (string? name-or-context)
          cref
          (cons mode (transform-context-declarations name-or-context transformations)))))

  (define (transform-declarations context)
    (for/fold ([c context])
              ([tr transformations])
        ((apply-transformation tr) c)))

  (~> context
      (hash-update 'includes transform-included-contexts)
      transform-declarations))

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
    [(list 'asset-prefix prefix)
     (asset-prefix decls prefix)]
    [(list 'real->float fp-sort)
     (real->float decls fp-sort)]))

;; Replace sorts by applying sort-transformer to every sort in a context

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
      [(list (and type-tag (or 'equation 'rule 'transformation))
             vars term1 term2 condition)
       (list type-tag (transform-vars vars) term1 term2 condition)]
      [(list 'assets assets)
       (list 'assets
             (for/hash ([(label value) assets])
               (values label (transform-item value))))]
      [(or (list 'as-equation _)
           (list 'as-rule _ _)
           (list 'substitute _ _)
           (list 'transform _ _))
       item]
      [term
       term]))

  (define (transform-rules rules)
    (map transform-item rules))

  (define (transform-assets assets)
    (second (transform-item (list 'assets assets))))

  (~> context
      (hash-set 'locs (hash))
      (hash-update 'sorts transform-sorts)
      (hash-update 'subsorts transform-subsorts)
      (hash-update 'vars transform-vars)
      (hash-update 'ops transform-ops)
      (hash-update 'rules transform-rules)
      (hash-update 'assets transform-assets)))

;; Add include (use/extend)

(define (add-include context mode cname-or-context)
  (hash-update context 'includes
               (λ (crefs) (append crefs (list (cons mode cname-or-context))))))

;; Add a prefix to all asset labels

(define (asset-prefix context prefix)

  (define (prefixed-label label)
    (string->symbol
     (string-append
      (symbol->string prefix)
      "."
      (symbol->string label))))

  (define (transform-item item)
    (match item
      [(list 'as-equation asset-ref)
       (list 'as-equation (prefixed-label asset-ref))]
      [(list 'as-rule asset-ref flip?)
       (list 'as-rule (prefixed-label asset-ref) flip?)]
      [(list (and type-tag (or 'substitute 'transform))
             rule-ref asset-ref reduce?)
       (list type-tag (prefixed-label rule-ref) (prefixed-label asset-ref) reduce?)]
      [(list 'assets assets)
       (list 'assets
             (for/hash ([(label value) assets])
               (values label (transform-item value))))]
      [other
       other]))

  (hash-update context 'assets
               (λ (assets) (hash prefix (transform-item (list 'assets assets))))))

;; Convert exact arithmetic on reals to inexact arithmetic on floats
;;
;; The principle is to replace all subsorts of ℝ that are not subsorts of ℤ
;; by FP32 or FP64. Two additional modifications are required:
;;
;;  1. Rational constants are converted to float. Integer constants are converted
;;     to float if they take the place of a rational value that just happens to be
;;     an integer.
;;  2. Rewrite rules that replace a float expression by an integer expression
;;     are converted by adding an int->float conversion on the replacement term.

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

  (define (transform-equation eq)
    (match-define (list (and type-tag (or 'equation 'rule 'transformation))
                        vars left right condition) eq)
    (define-values (lsort mod-left) (transform-term left vars))
    (define-values (rsort mod-right) (transform-term right vars))
    (define-values (csort mod-condition) (transform-term condition vars))
    (if (or (real-sort? lsort) (real-sort? rsort))
        (list type-tag vars
              (transform-literal mod-left rsort)
              (transform-literal mod-right lsort)
              mod-condition)
        (list type-tag vars mod-left mod-right mod-condition)))

  (define (transform-item item)
    (match item
      [(list (or 'equation 'rule 'transformation) args ...)
       (transform-equation item)]
      [(list 'assets assets)
       (list 'assets (for/hash ([(label value) assets])
                       (values label (transform-item value))))]
      [(or (list 'as-equation _)
           (list 'as-rule _ _)
           (list 'substitute _ _)
           (list 'transform _ _))
       item]
      [term
       (define-values (t-sort t-term) (transform-term term (hash)))
       t-term]))

  (~> context
      (hash-update 'rules (λ (rules) (map transform-equation rules)))
      (hash-update 'assets (λ (assets) (second (transform-item (list 'assets assets)))))
      (replace-sorts transform-sort)
      (add-include 'use "builtins/IEEE-floating-point")))
