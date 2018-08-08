#lang racket

(provide
 (struct-out context)
 (contract-out
  [xexpr->context (xexpr/c . -> . (values string? context?))]
  [add-implicit-declarations (context? . -> . context?)]))


(require "./lightweight-class.rkt"
         ;; (prefix-in sorts: "./sorts.rkt")
         ;; (prefix-in operators: "./operators.rkt")
         ;; (prefix-in terms: "./terms.rkt")
         ;; (prefix-in equations: "./equations.rkt")
         ;; (prefix-in rewrite: "./rewrite.rkt")
         ;; (prefix-in tools: "./tools.rkt")
         ;; (prefix-in builtins: "./builtin-contexts.rkt")
         (only-in racket/hash hash-union)
         xml
         threading)

(module+ test
  (require rackunit))

;;
;; A context is the main unit of Leibniz code. It defines a term algebra
;; (sort graph, operators, variables) and a list of rewrite rules,
;; plus optional names values (terms, equations, or rules) called assets
;;

(define-class context

  (field includes sorts subsorts vars ops rules assets locs
         compiled-signature compiled-rules compiled-assets)

  ; includes: a list of include declarations
  ; sorts: a set of declared sorts                          
  ; subsorts: a set of subsort declarations (pairs of sorts)   
  ; vars: a hash mapping var names to sorts                
  ; ops: a set of op declarations (sexps)                 
  ; rules: a list of rules (sexps)                          
  ; assets: a hash mapping labels to assets (sexps)          
  ; locs: a hash mapping declarations to source locations  
  ; compiled-signature: optimized representation of sorts, subsorts, vars, and ops
  ; compiled-rules: optimized representation of rules
  ; compiled-assets: optimized representation of assets

)

(define empty-context
  (context empty (set) (set)  (hash) (set) (list) (hash) (hash)
           #f #f #f))

;;
;; Create a context from an xexpr
;;

(define (combine-varsets name sort1 sort2)
  (unless (equal? sort1 sort2)
    (error (format "Var ~a of sort ~a redefined with sort ~a"
                   name sort1 sort2)))
  sort1)

(define (xexpr->context xexpr-context)

  (define (xexpr->arg xexpr-arg)
    (match xexpr-arg
      [`(var ((id ,var-name) (sort ,sort-name)))
       (list 'var (string->symbol var-name) (string->symbol sort-name))]
      [`(var ((sort ,sort-name) (id ,var-name)))
       (list 'var (string->symbol var-name) (string->symbol sort-name))]
      [`(sort ((id ,sort-name)))
       (list 'sort (string->symbol sort-name))]))

  (define (xexpr->vars xexpr-vars)
    (for/fold ([vars (hash)])
              ([v xexpr-vars])
      (define new-var
        (match v
          [(or `(var ((id ,vn) (sort ,sn)))
               `(var ((sort ,sn) (id ,vn))))
           (hash (string->symbol vn) (string->symbol sn))]))
      (hash-union vars new-var #:combine/key combine-varsets)))

  (define (xexpr->asset xexpr-asset)
    (match xexpr-asset
      [`(assets () ... (asset ((id ,label)) ,value) ...)
       (list 'assets
             (for/hash ([l label]
                        [v value])
               (values (string->symbol l) (xexpr->asset v))))]
      [`(equation () ...
                  (vars ,ev ...)
                  (left ,el)
                  ,ec
                  (right ,er))
       (list 'equation
             (xexpr->vars ev)
             (xexpr->asset el)
             (xexpr->asset er)
             (match ec
               [`(condition)       #f]
               [`(condition ,term) term]))]
      [`(rule () ...
              (vars () ... ,rv ...)
              (pattern () ... ,rp)
              ,rc
              (replacement () ... ,rr))
       (list 'rule
             (xexpr->vars rv)
             (xexpr->asset rp)
             (xexpr->asset rr)
             (match rc
               [`(condition () ...)       #f]
               [`(condition () ... ,term) term]))]
      [`(transformation () ...
                        (vars () ... ,rv ...)
                        (pattern () ... ,rp)
                        ,rc
                        (replacement () ... ,rr))
       (list 'transformation
             (xexpr->vars rv)
             (xexpr->asset rp)
             (xexpr->asset rr)
             (match rc
               [`(condition () ...)       #f]
               [`(condition () ... ,term) term]))]
      [`(term ((op ,op-string)) ,args ...)
       (list 'term
             (string->symbol op-string)
             (map xexpr->asset args))]
      [`(term-or-var ((name ,name-string)))
       (list 'term-or-var (string->symbol name-string))]
      [`(,number-tag ((value ,v)))
       (list number-tag (read (open-input-string v)))]
      [`(as-equation ((ref ,asset-ref)))
       (list 'as-equation (string->symbol asset-ref))]
      [`(as-rule ((ref ,asset-ref) (flip ,flip?)))
       (list 'as-rule (string->symbol asset-ref) (equal? flip? "true"))]
      [`(substitute () ...
                    ((substitute ,substitution-ref)
                     (ref ,asset-ref)
                     (reduce ,reduce?)))
       (list 'substitute (string->symbol substitution-ref)
                         (string->symbol asset-ref)
                         (equal? reduce? "true"))]
      [`(transform () ...
                   ((transformation ,transformation-ref)
                    (ref ,asset-ref)
                    (reduce ,reduce?)))
       (list 'transform (string->symbol transformation-ref)
                        (string->symbol asset-ref)
                        (equal? reduce? "true"))]))

  (match-define
    `(context ((id ,name))
              (includes () ... ,xexpr-includes ...)
              (sorts () ... ,xexpr-sorts ...)
              (subsorts () ... ,xexpr-subsorts ...)
              (vars () ... ,xexpr-vars ...)
              (ops () ... ,xexpr-ops ...)
              (rules () ... ,xexpr-rules ...)
              (assets () ... ,xexpr-assets ...))
    xexpr-context)
 
  (define includes (for/list ([include xexpr-includes])
                     (match include
                       [(list mode include-ref)
                        (cons mode include-ref)])))
  (define sorts (for/set ([sort xexpr-sorts])
                  (match sort
                    [`(sort ((id ,s)))
                     (string->symbol s)])))
  (define subsorts (for/set ([subsort xexpr-subsorts])
                     (match subsort
                       [`(subsort ((subsort ,s1) (supersort ,s2)))
                        (cons (string->symbol s1) (string->symbol s2))]
                       [`(subsort ((supersort ,s2) (subsort ,s1)))
                        (cons (string->symbol s1) (string->symbol s2))])))
  (define vars (xexpr->vars xexpr-vars))
  (define ops (for/set ([op xexpr-ops])
                (match op
                  [`(op ((id ,on))
                        (arity () ... ,oa ...)
                        (sort ((id ,os))))
                   (list (string->symbol on)
                         (map xexpr->arg oa)
                         (string->symbol os))])))
  (define rules (for/list ([rule xexpr-rules])
                  (xexpr->asset rule)))
  (define assets (second (xexpr->asset (cons 'assets xexpr-assets))))

  (values name
          (context includes sorts subsorts vars ops rules assets (hash)
                   #f #f #f)))

(module+ test

  (let-values ([(name the-context)
                (xexpr->context '(context ((id "test"))
                                          (includes)
                                          (sorts)
                                          (subsorts)
                                          (vars)
                                          (ops)
                                          (rules)
                                          (assets)))])
    (check-equal? name "test")
    (check-equal? the-context empty-context))

  (define the-xexpr-context
    '(context ((id "test"))
              (includes)
              (sorts (sort ((id "foo")))
                     (sort ((id "bar"))))
              (subsorts (subsort ((subsort "foo")
                                  (supersort "bar"))))
              (vars (var ((id "X") (sort "foo"))))
              (ops (op ((id "a-foo")) (arity) (sort ((id "foo"))))
                   (op ((id "a-bar")) (arity) (sort ((id "bar"))))
                   (op ((id "foo2bar")) (arity (sort ((id "foo")))) (sort ((id "bar")))))
              (rules (rule (vars)
                           (pattern (term ((op "foo2bar"))
                                          (term-or-var ((name "a-bar")))))
                           (condition)
                           (replacement (term-or-var ((name "a-foo"))))))
              (assets (asset ((id "a-term"))
                             (term-or-var ((name "a-foo")))))))

  (define the-reference-context
    (context empty
             (set 'foo 'bar)
             (set (cons 'foo 'bar))
             (hash 'X 'foo)
             (set '(a-foo () foo)
                  '(a-bar () bar)
                  '(foo2bar ((sort foo)) bar))
             (list (list 'rule (hash)
                         '(term foo2bar ((term-or-var a-bar)))
                         '(term-or-var a-foo) #f))
             (hash 'a-term '(term-or-var a-foo))
             (hash) #f #f #f))

  (let-values ([(name the-context) (xexpr->context the-xexpr-context)])
    (check-equal? name "test")
    (check-equal? the-context the-reference-context))

  (let-values ([(name the-context)
                (~> the-xexpr-context
                    xexpr->string
                    string->xexpr
                    xexpr->context)])
    (check-equal? name "test")
    (check-equal? the-context the-reference-context)))

;;
;; Take a context generated from a source file, and add
;; some implicit declarations to spare authors the effort
;; to add them explicitly to their prose.
;;
(define (add-implicit-declarations cntxt)
  (define sorts
    (set-union ; the original sort declarations
               (context-sorts cntxt)
               ; both sets referenced in a subsort declaration
               (apply set-union (set)
                      (for/list ([pair (context-subsorts cntxt)])
                        (set (car pair) (cdr pair))))
               ; the result sorts of all op declarations
               (apply set (for/list ([op (context-ops cntxt)])
                            (third op)))))

  (define vars
    (apply hash-union ; the original var declarations
                      (context-vars cntxt)
                      ; the vars defined in operator argument lists
                      (for*/list ([op (context-ops cntxt)]
                                  [arg (second op)]
                                  #:when (equal? 'var (first arg)))
                        (hash (second arg) (third arg)))
                      ; combine avoiding name clashes
                      #:combine/key combine-varsets))

  (context (context-includes cntxt)
           sorts
           (context-subsorts cntxt)
           vars
           (context-ops cntxt)
           (context-rules cntxt)
           (context-assets cntxt)
           (context-locs cntxt)
           #f #f #f))

(module+ test

  (check-equal? (add-implicit-declarations the-reference-context)
                the-reference-context)

  (define a-full-context
    (context empty
             (set 'foo 'bar 'baz)
             (set (cons 'foo 'bar))
             (hash 'X 'bar)
             (set '(a-foo () foo)
                  '(a-bar () bar)
                  '(a-baz () baz)
                  '(bar2foo ((var X bar)) foo))
             empty
             (hash)
             (hash) #f #f #f))

  (define a-minimal-context
    (context empty
             (set)
             (set (cons 'foo 'bar))
             (hash)
             (set '(a-foo () foo)
                  '(a-bar () bar)
                  '(a-baz () baz)
                  '(bar2foo ((var X bar)) foo))
             empty
             (hash)
             (hash) #f #f #f))

  (check-equal? (add-implicit-declarations a-minimal-context)
                a-full-context))
