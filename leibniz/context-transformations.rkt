#lang racket

(provide
 (contract-out
  [replace-sort (context? string? string? . -> . context?)]
  [replace-sort-prefix (context? string? string? . -> . context?)]))

(require "./contexts.rkt"
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

  (define transformed-context
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
             (hash)))

  (check-equal? (replace-sort test-context "bar" "baz")
                transformed-context))
