#lang racket

(provide
 (contract-out
  [replace-sort (context? string? string? . -> . context?)]
  [replace-sort-prefix (context? string? string? . -> . context?)]
  [replace-include (context? string? string? . -> . context?)]
  [remove-vars (context? . -> . context?)]))

(require "./contexts.rkt"
         racket/hash
         threading)

(module+ test
  (require rackunit
           racket/function)

  (define test-context
    (context empty
             (set 'foo 'bar)
             (set (cons 'foo 'bar))
             (hash 'X 'foo
                   'Y 'bar)
             (set '(a-foo () foo)
                  '(a-foo ((sort bar)) foo)
                  '(a-bar () bar)
                  '(foo2bar ((var X foo)) bar)
                  '(bar2foo ((var Y bar)) foo))
             (list (list 'rule (hash)
                         '(term foo2bar ((term-or-var X)))
                         '(term-or-var a-bar) #f)
                   (list 'rule (hash)
                         '(term bar2foo ((term-or-var Y)))
                         '(term a-foo ((term-or-var Y))) #f))
             (hash)))

  (define compiled-test-context
    (compile-context test-context (λ (name) (error "don't call this")))))

;;
;; Sort replacement by name or by prefix
;;
(define (replace-sorts cntxt sort-transformer)

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

  (context (context-includes cntxt)
           (transform-sorts (context-sorts cntxt))
           (transform-subsorts (context-subsorts cntxt))
           (transform-vars (context-vars cntxt))
           (transform-ops (context-ops cntxt))
           (transform-rules (context-rules cntxt))
           (transform-assets (context-assets cntxt))))

(define (replace-prefix sort-symbol prefix-str replacement-str)
  (define sort-str (symbol->string sort-symbol))
  (if (string-prefix? sort-str prefix-str)
      (string->symbol (string-append replacement-str
                                     (substring sort-str (string-length prefix-str))))
      sort-symbol))

(define (replace-sort cntxt current-sort-str new-sort-str)
  (define current-sort (string->symbol current-sort-str))
  (define new-sort (string->symbol new-sort-str))
  (replace-sorts cntxt (λ (s) (if (equal? s current-sort) new-sort s))))

(define (replace-sort-prefix cntxt current-prefix new-prefix)
  (replace-sorts cntxt (λ (s) (replace-prefix s current-prefix new-prefix))))

(module+ test

  (check-equal? (replace-prefix 'a "b" "c")
                'a)
  (check-equal? (replace-prefix 'a "a" "c")
                'c)
  (check-equal? (replace-prefix 'a.b "b" "c")
                'a.b)
  (check-equal? (replace-prefix 'a.b "a" "c")
                'c.b)

  (check-equal? (replace-sort test-context "bar" "baz")
                (context empty
                         (set 'foo 'baz)
                         (set (cons 'foo 'baz))
                         (hash 'X 'foo
                               'Y 'baz)
                         (set '(a-foo () foo)
                              '(a-foo ((sort baz)) foo)
                              '(a-bar () baz)
                              '(foo2bar ((var X foo)) baz)
                              '(bar2foo ((var Y baz)) foo))
                         (list (list 'rule (hash)
                                     '(term foo2bar ((term-or-var X)))
                                     '(term-or-var a-bar) #f)
                               (list 'rule (hash)
                                     '(term bar2foo ((term-or-var Y)))
                                     '(term a-foo ((term-or-var Y))) #f))
                         (hash))))

;;
;; Replace includes
;;
(define (replace-include cntxt current-include new-include)

  (define transformed-includes
    (for/list ([mode/name (context-includes cntxt)])
      (match-define (cons mode name) mode/name)
      (cons mode (if (equal? name current-include)
                     new-include
                     name))))

  (struct-copy context cntxt
               [includes transformed-includes]))

(module+ test
  (define cntxt1 (struct-copy context empty-context
                              [includes '((use . "foo")
                                          (extend . "baz"))]))
  (define cntxt2 (struct-copy context empty-context
                              [includes '((use . "bar")
                                          (extend . "baz"))]))
  (check-equal? (replace-include cntxt1 "foo" "bar")
                cntxt2))

;;
;; Remove context-level vars by pushing them into rule/equation definitions
;;
(define (remove-vars cntxt)

  (define vars-from-context (context-vars cntxt))

  (define (combined-referenced-vars local-vars referenced-vars)
    ;; The context var is added only if no local var of the same name
    ;; already exists and if the var is actually referenced in a term.
    (for/fold ([vars local-vars])
              ([(cvar-name cvar-sort)  vars-from-context])
      (if (and (set-member? referenced-vars cvar-name)
               (not (hash-has-key? local-vars cvar-name)))
          (hash-set vars cvar-name cvar-sort)
          vars)))

  (define (remove-vars-from-op op-decl)
    (match-define (list op arity v-sort) op-decl)
    (list op
          (for/list ([op-arg arity])
            (match op-arg
              [(list 'var name sort)
               (if (hash-has-key? vars-from-context name)
                   (list 'sort sort)
                   op-arg)]
              [else
               op-arg]))
          v-sort))

  (define (collect-referenced-vars term [acc (set)])
    (match term
      [(list 'term-or-var name)
       (set-add acc name)]
      [(list 'term op args)
       (for/fold ([acc acc])
                 ([arg args])
         (collect-referenced-vars arg acc))]
      [else
       acc]))

  (define (remove-vars-from-item item)
    (match item
      [(list (and type-tag (or 'equation 'rule 'transformation))
             vars term1 term2 condition)
       ;; No need to scan condition because it cannot use vars that
       ;; are not referenced in either term.
       (define referenced-vars (set-union (collect-referenced-vars term1)
                                          (collect-referenced-vars term2)))
       (list type-tag (combined-referenced-vars vars referenced-vars) term1 term2 condition)]
      [(list 'assets assets)
       (list 'assets
             (for/hash ([(label value) assets])
               (values label (remove-vars-from-item value))))]
      [(or (list 'as-equation _)
           (list 'as-rule _ _)
           (list 'substitute _ _)
           (list 'transform _ _))
       item]
      [(list 'term-or-var name)
       (when (hash-has-key? vars-from-context name)
         (error (format "cannot remove var ~a because it is used in a term asset" name)))
       item]
      [(list 'term op args)
       (list 'term op (map remove-vars-from-item args))]
      [other-term
       other-term]))

  (if (hash-empty? vars-from-context)
      cntxt
      (struct-copy context cntxt
                   [vars (hash)]
                   [ops (for/set ([op (context-ops cntxt)])
                          (remove-vars-from-op op))]
                   [rules (for/list ([r (context-rules cntxt)])
                            (remove-vars-from-item r))]
                   [assets (for/hash ([(label value) (context-assets cntxt)])
                             (values label (remove-vars-from-item value)))])))

(module+ test
  (check-equal? (remove-vars test-context)
                (context empty
                         (set 'foo 'bar)
                         (set (cons 'foo 'bar))
                         (hash)
                         (set '(a-foo () foo)
                              '(a-foo ((sort bar)) foo)
                              '(a-bar () bar)
                              '(foo2bar ((sort foo)) bar)
                              '(bar2foo ((sort bar)) foo))
                         (list (list 'rule (hash 'X 'foo)
                                     '(term foo2bar ((term-or-var X)))
                                     '(term-or-var a-bar) #f)
                               (list 'rule (hash 'Y 'bar)
                                     '(term bar2foo ((term-or-var Y)))
                                     '(term a-foo ((term-or-var Y))) #f))
                         (hash))))
