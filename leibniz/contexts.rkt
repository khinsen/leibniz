#lang racket

(provide
 (struct-out exn:fail:leibniz)
 re-raise-exn
 (contract-out
  [context? (any/c . -> . boolean?)]
  [empty-context context?]
  [xexpr->context+name (xexpr/c . -> . (values context? (or/c #f string?)))]
  [xexpr->context (xexpr/c . -> . context?)]
  [context->xexpr ((context?) (string?) . ->* . xexpr/c)]
  [add-implicit-declarations (context? . -> . context?)]
  [compile-context (context? (string? . -> . context?)  . -> . context?)]
  [without-compiled-resources (context? . -> . context?)]
  [make-builtin-context
         ((listof pair?) operators:signature? equations:rulelist?
         . -> . context?)]
  [check-asset (context? xexpr/c . -> . any/c)]
  [eval-asset (context? xexpr/c . -> . any/c)]))

(require "./lightweight-class.rkt"
         (prefix-in sorts: "./sorts.rkt")
         (prefix-in operators: "./operators.rkt")
         (prefix-in terms: "./terms.rkt")
         (prefix-in equations: "./equations.rkt")
         (prefix-in rewrite: "./rewrite.rkt")
         (only-in racket/hash hash-union)
         xml
         threading)

(module+ test
  (require rackunit
           racket/function)

  (define (dummy-name-resolver name)
    (error "Call of dummy function"))

  (define xexpr-context
    '(context ((id "test"))
              (includes)
              (sorts (sort ((id "foo")))
                     (sort ((id "bar"))))
              (subsorts (subsort ((subsort "foo")
                                  (supersort "bar"))))
              (vars (var ((id "X") (sort "foo"))))
              (ops (op ((id "a-foo")) (arity) (sort ((id "foo"))))
                   (op ((id "a-foo")) (arity (sort ((id "bar")))) (sort ((id "foo"))))
                   (op ((id "a-bar")) (arity) (sort ((id "bar"))))
                   (op ((id "foo2bar"))
                       (arity (var ((id "X") (sort "foo"))))
                       (sort ((id "bar")))))
              (rules (rule (vars)
                           (pattern (term ((op "foo2bar"))
                                          (term-or-var ((name "a-bar")))))
                           (condition)
                           (replacement (term-or-var ((name "a-foo"))))))
              (assets (asset ((id "a-term"))
                             (term-or-var ((name "a-foo"))))
                      (asset ((id "eq1"))
                             (equation (vars)
                                       (left (term-or-var ((name "a-foo"))))
                                       (condition)
                                       (right (term ((op "a-foo"))
                                                    (term-or-var ((name "a-foo")))))))
                      (asset ((id "tr1"))
                             (transformation (vars)
                                             (pattern (term-or-var ((name "a-foo"))))
                                             (condition)
                                             (replacement (term ((op "a-foo"))
                                                                (term-or-var
                                                                 ((name "a-foo")))))))
                      (asset ((id "nested"))
                             (assets (asset ((id "asset"))
                                            (term-or-var ((name "a-foo"))))))
                      (asset ((id "deeply"))
                             (assets
                              (asset ((id "nested"))
                                     (assets (asset ((id "asset"))
                                                    (term-or-var ((name "a-foo"))))))))
                      )))

  (define reference-context
    (context empty
             (set 'foo 'bar)
             (set (cons 'foo 'bar))
             (hash 'X 'foo)
             (set '(a-foo () foo)
                   '(a-foo ((sort bar)) foo)
                   '(a-bar () bar)
                   '(foo2bar ((var X foo)) bar))
             (list (list 'rule (hash)
                         '(term foo2bar ((term-or-var a-bar)))
                         '(term-or-var a-foo) #f))
             (hash 'a-term '(term-or-var a-foo)
                   'eq1 (list 'equation
                              (hash)
                              '(term-or-var a-foo)
                              '(term a-foo ((term-or-var a-foo)))
                              #f)
                   'tr1 (list 'transformation
                              (hash)
                              '(term-or-var a-foo)
                              '(term a-foo ((term-or-var a-foo)))
                              #f)
                   'nested (list 'assets (hash 'asset
                                               '(term-or-var a-foo)))
                   'deeply (list 'assets (hash 'nested
                                               (list 'assets
                                                     (hash 'asset
                                                           '(term-or-var a-foo))))))
             #f #f #f)))

;;
;; A context is the main unit of Leibniz code. It defines a term algebra
;; (sort graph, operators, variables) and a list of rewrite rules,
;; plus optional names values (terms, equations, or rules) called assets
;;

(define-class context

  (field includes sorts subsorts vars ops rules assets
         compiled-signature compiled-rules compiled-assets)

  ; includes: a list of include declarations
  ; sorts: a set of declared sorts                          
  ; subsorts: a set of subsort declarations (pairs of sorts)   
  ; vars: a hash mapping var names to sorts                
  ; ops: a set of op declarations (sexps)                 
  ; rules: a list of rules (sexps)                          
  ; assets: a hash mapping labels to assets (sexps)          
  ; compiled-signature: optimized representation of sorts, subsorts, vars, and ops
  ; compiled-rules: optimized representation of rules
  ; compiled-assets: optimized representation of assets

)

(define empty-context
  (context empty (set) (set)  (hash) (set) (list) (hash)
           #f #f #f))

;;
;; Create a context from an xexpr
;;

(define (combine-varsets name sort1 sort2)
  (unless (equal? sort1 sort2)
    (error (format "Var ~a of sort ~a redefined with sort ~a"
                   name sort1 sort2)))
  sort1)


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
                (vars () ... ,ev ...)
                (left () ... ,el)
                ,ec
                (right () ... ,er))
     (list 'equation
           (xexpr->vars ev)
           (xexpr->asset el)
           (xexpr->asset er)
           (match ec
             [`(condition () ...)       #f]
             [`(condition () ... ,term) term]))]
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
           (equal? reduce? "true"))]
    [`(test () ...
            (term ,t)
            (reduced-term ,rt))
     (list 'test (xexpr->asset t) (xexpr->asset rt))]))

(define (xexpr->context+name xexpr-context)

  (match-define
    `(context (,attrs ...) ...
              (includes () ... ,xexpr-includes ...)
              (sorts () ... ,xexpr-sorts ...)
              (subsorts () ... ,xexpr-subsorts ...)
              (vars () ... ,xexpr-vars ...)
              (ops () ... ,xexpr-ops ...)
              (rules () ... ,xexpr-rules ...)
              (assets () ... ,xexpr-assets ...))
    xexpr-context)

  (define name (if (and (equal? 1 (length attrs))
                        (equal? 1 (length (first attrs)))
                        (equal? 2 (length (first (first attrs))))
                        (equal? (first (first (first attrs))) 'id))
                   (second (first (first attrs)))
                   #f))
  (define includes (for/list ([include xexpr-includes])
                     (match include
                       [`(include ((mode ,mode) (ref ,ref)))
                        (cons (string->symbol mode) ref)]
                       [`(include ((ref ,ref) (mode ,mode)))
                        (cons (string->symbol mode) ref)])))
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

  (values (context includes sorts subsorts vars ops rules assets
                   #f #f #f)
          name))

(define (xexpr->context xexpr-context)
  (define-values (cntxt name)
    (xexpr->context+name xexpr-context))
  cntxt)

(module+ test

  (check-equal? (xexpr->context '(context (includes)
                                          (sorts)
                                          (subsorts)
                                          (vars)
                                          (ops)
                                          (rules)
                                          (assets)))
                empty-context)

  (check-equal? (xexpr->context xexpr-context)
                reference-context)

  (let-values ([(cntxt name)
                (~> xexpr-context
                    xexpr->string
                    string->xexpr
                    xexpr->context+name)])
    (check-equal? name "test")
    (check-equal? cntxt reference-context)))

;;
;; Make an xexpr representation of a context
;;
(define (context->xexpr cntxt [name #f])

  (define (vars->xexpr vars)
    (for/list ([(name sort) vars])
      `(var ((id ,(symbol->string name))
               (sort ,(symbol->string sort))))))

  (define (op->xexpr op)
    `(op ((id ,(symbol->string (first op))))
         (arity ,@(for/list ([sv (second op)])
                    (if (equal? (first sv) 'sort)
                        `(sort ((id ,(symbol->string (second sv)))))
                        `(var ((id ,(symbol->string (second sv)))
                                 (sort ,(symbol->string (third sv))))))))
         (sort ((id ,(symbol->string (third op)))))))

  (define (condition->xexpr condition)
    (if (equal? condition #f)
        `(condition)
        `(condition ,(asset->xexpr condition))))

  (define (asset->xexpr asset)
    (match asset
      [(list 'assets asset-data)
       (assets->xexpr asset-data)]
      [(list 'equation vars left right condition)
       `(equation (vars ,@(vars->xexpr vars))
                  (left ,(asset->xexpr left))
                  ,(condition->xexpr condition)
                  (right ,(asset->xexpr right)))]
      [(list 'rule vars pattern replacement condition)
       `(rule (vars ,@(vars->xexpr vars))
              (pattern ,(asset->xexpr pattern))
              ,(condition->xexpr condition)
              (replacement ,(asset->xexpr replacement)))]
      [(list 'transformation vars pattern replacement condition)
       `(transformation (vars ,@(vars->xexpr vars))
                        (pattern ,(asset->xexpr pattern))
                        ,(condition->xexpr condition)
                        (replacement ,(asset->xexpr replacement)))]
      [(list 'term-or-var name)
       `(term-or-var ((name ,(symbol->string name))))]
      [(list 'term op args)
       `(term ((op ,(symbol->string op)))
              ,@(for/list ([arg args])
                  (asset->xexpr arg)))]
      [(list (and number-tag (or 'integer 'rational 'floating-point)) x)
       `(,number-tag ((value ,(format "~a" x))))]
      [(list 'as-equation asset-ref)
       `(as-equation ((ref ,(symbol->string asset-ref))))]
      [(list 'as-rule asset-ref flip?)
       `(as-rule ((ref ,(symbol->string asset-ref))
                    (flip ,(if flip? "true" "false"))))]
      [(list 'substitute substitution-ref asset-ref reduce?)
       `(substitute ((substitute ,(symbol->string substitution-ref))
                       (ref ,(symbol->string asset-ref))
                       (reduce ,(if reduce? "true" "false"))))]
      [(list 'transform transformation-ref asset-ref reduce?)
       `(transform ((transformation ,(symbol->string transformation-ref))
                      (ref ,(symbol->string asset-ref))
                      (reduce ,(if reduce? "true" "false"))))]))

  (define (assets->xexpr assets)
    `(assets ,@(for/list ([(label asset) assets])
                 `(asset ((id ,(symbol->string label))) ,(asset->xexpr asset)))))

  `(context ((id ,name))
            (includes ,@(for/list ([mode/name (context-includes cntxt)])
                          `(include ((mode ,(symbol->string (car mode/name)))
                                     (ref ,(cdr mode/name))))))
            (sorts ,@(for/list ([s (context-sorts cntxt)])
                       `(sort ((id ,(symbol->string s))))))
            (subsorts ,@(for/list ([sd (context-subsorts cntxt)])
                          `(subsort ((subsort ,(symbol->string (car sd)))
                                     (supersort ,(symbol->string (cdr sd)))))))
            (vars ,@(vars->xexpr (context-vars cntxt)))
            (ops ,@(for/list ([od (context-ops cntxt)])
                     (op->xexpr od)))
            (rules ,@(for/list ([rd (context-rules cntxt)])
                       (asset->xexpr rd)))
            ,(assets->xexpr (context-assets cntxt))))

(module+ test
  (let-values ([(cntxt name)
                (~> reference-context
                    (context->xexpr "test")
                    xexpr->context+name)])
    (check-equal? name "test")
    (check-equal? cntxt reference-context)))

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
           #f #f #f))

(module+ test

  (check-equal? (add-implicit-declarations reference-context)
                reference-context)

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
             #f #f #f))

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
             #f #f #f))

  (check-equal? (add-implicit-declarations a-minimal-context)
                a-full-context)
  (check-equal? (add-implicit-declarations a-full-context)
                a-full-context))

;;
;; Re-raise exceptions with the declaration that caused it
;;
(struct exn:fail:leibniz exn:fail (a-decl))

(define ((re-raise-exn decl) e)
  (if decl
      (raise (exn:fail:leibniz
              (exn-message e)
              (current-continuation-marks)
              decl))
      (raise e)))

;;
;; Compile a context. This generates optimized internal representations for
;; signatures, rule lists, and assets, which are cached.
;;
(define (compile-term signature)
  (letrec ([fn (match-lambda
                 [(list 'term-or-var name)
                  (terms:make-var-or-term signature name)]
                 [(list 'term op args)
                  (terms:make-term signature op (map fn args))]
                 [(list 'integer n) n]
                 [(list 'rational r) r]
                 [(list 'floating-point fp) fp])])
    fn))

(define (compile-pattern signature local-vars)
  (letrec ([fn (match-lambda
                 [#f
                  #f]
                 [(list 'term-or-var name)
                  (terms:make-var-or-term signature name local-vars)]
                 [(list 'term op args)
                  (terms:make-term signature op (map fn args))]
                 [(list 'integer n) n]
                 [(list 'rational r) r]
                 [(list 'floating-point fp) fp])])
    fn))

(define (compile-rule signature rule-expr check-equationality?)
  (match rule-expr
    [(list 'rule vars pattern replacement condition)
     (let* ([mp (compile-pattern signature vars)])
       (equations:make-rule signature (mp pattern)
                            (mp condition)
                            (mp replacement)
                            #f
                            check-equationality?))]
    [_ #f]))

(define (compile-transformation signature rule-expr)
  (and~> (compile-rule signature (cons 'rule (rest rule-expr)) #f)
         (equations:make-transformation signature _)))

(define (compile-equation signature equation-expr)
  (match equation-expr
    [(list 'equation vars left right condition)
     (let ([mp (compile-pattern signature vars)])
       (equations:make-equation signature
                                (mp left)
                                (mp condition)
                                (mp right)))]
    [_ #f]))

(define (compile-context cntxt name-resolver)

  (define includes
    (for/list ([mode/name (context-includes cntxt)])
      (cons (car mode/name) (name-resolver (cdr mode/name)))))

  (define (compile-sort-graph)
    ;; Merge the sort graphs of the included contexts.
    (define after-includes
      (for/fold ([ms sorts:empty-sort-graph])
                ([m/c includes])
        (sorts:merge-sort-graphs ms
                                 (operators:signature-sort-graph
                                  (context-compiled-signature (cdr m/c))))))
    ;; Process the sort declarations.
    (define after-sorts
      (for/fold ([sorts after-includes])
                ([s (context-sorts cntxt)])
        (with-handlers ([exn:fail? (re-raise-exn `(sort ,s))])
          (sorts:add-sort sorts s))))
    ;; Process the subsort declarations.
    (for/fold ([sorts after-sorts])
              ([ss (context-subsorts cntxt)])
      (with-handlers ([exn:fail? (re-raise-exn `(subsort ,ss))])
        (sorts:add-subsort-relation sorts (car ss) (cdr ss)))))

  (define sort-graph (compile-sort-graph))

  (define (compile-signature)
    (define (argsort sort-or-var-decl)
      (match sort-or-var-decl
        [(list 'sort sort-id) sort-id]
        [(list 'var var-name sort-id) sort-id]))
    ;; Merge the signatures of the included contexts.
    (define after-includes
      (for/fold ([msig (operators:empty-signature sort-graph)])
                ([m/c includes])
        (define csig (context-compiled-signature (cdr m/c)))
        (define isig (case (car m/c)
                       [(use) (operators:remove-vars csig)]
                       [(extend) csig]))
        (operators:merge-signatures msig isig sort-graph)))
    ;; Process the op declarations.
    (define after-ops
      (for/fold ([sig after-includes])
                ([od (context-ops cntxt)])
        (with-handlers ([exn:fail? (re-raise-exn (cons 'op od))])
          (match-define (list name arity rsort) od)
          (operators:add-op sig name (map argsort arity) rsort))))
    ;; Process the var declarations.
    (define signature
      (for/fold ([sig after-ops])
                ([(vname vsort) (context-vars cntxt)])
        (with-handlers ([exn:fail? (re-raise-exn `(var ,vname ,vsort))])
          (operators:add-var sig vname vsort))))
    ;; Check for non-regularity.
    (define non-regular (operators:non-regular-op-example signature))
    (when non-regular
      (match-let ([(list op arity rsorts) non-regular])
        (displayln (format "Warning: operator ~a~a has ambiguous sorts ~a"
                           op arity rsorts))))
    signature)

  (define signature (compile-signature))

  (define (compile-rules)
    ;; Merge the rule lists of the included contexts.
    (define after-includes
      (for/fold ([mrl equations:empty-rulelist])
                ([m/c includes])
        (equations:merge-rulelists mrl
                                   (context-compiled-rules (cdr m/c))
                                   signature)))
    ;; Process the rule declarations
    (for/fold ([rl after-includes])
              ([rd (context-rules cntxt)])
      (with-handlers ([exn:fail? (re-raise-exn rd)])
        (equations:add-rule rl (compile-rule signature rd #t)))))

  (define rules (compile-rules))

  (define (as-rule* signature value flip?)
    (cond
      [(equations:rule? value)
       value]
      [(equations:equation? value)
       (define-values (left right)
         (if flip?
             (values (equations:equation-right value)
                     (equations:equation-left value))
             (values (equations:equation-left value)
                     (equations:equation-right value))))
       (equations:make-rule signature
                            left
                            (equations:equation-condition value)
                            right
                            #f #t)]
      [else (error (format "cannot convert ~a to a rule" value))]))

  (define (as-equation* signature value)
    (cond
      [(equations:equation? value)
       value]
      [(equations:rule? value)
       (equations:make-equation signature
                                (equations:rule-pattern value)
                                (equations:rule-condition value)
                                (equations:rule-replacement value))]
      [else (error (format "cannot convert ~a to an equation" value))]))

  (define (substitution* signature rule value)
    (unless (equations:rule? rule)
      (error (format "not a rule: ~a" rule)))
    (define transformation (equations:make-transformation signature rule))
    (cond
      [(terms:term? value)
       (rewrite:substitute signature transformation value)]
      [(equations:equation? value)
       (rewrite:substitute-equation signature transformation value)]
      [else
       (error (format "not a term or equation: ~a" value))]))

  (define (transformation* signature transformation value)
    (unless (equations:transformation? transformation)
      (error (format "not a transformation: ~a" transformation)))
    (cond
      [(terms:term? value)
       (rewrite:transform signature transformation value)]
      [(equations:equation? value)
       (rewrite:transform-equation signature transformation value)]
      [else
       (error (format "not a term or equation: ~a" value))]))

  (define (reduce* signature rulelist value)
    (unless (equations:rulelist? rulelist)
      (error (format "not a rulelist: ~a" rulelist)))
    (cond
      [(terms:term? value)
       (rewrite:reduce signature rulelist value)]
      [(equations:equation? value)
       (rewrite:reduce-equation signature rulelist value)]
      [else
       (error (format "not a term or equation: ~a" value))]))

  (define (nested-labels compound-label)
    (~> (symbol->string compound-label)
        (string-split ".")
        (map string->symbol _)))

  (define (lookup-asset assets label)
    (define value
      (for/fold ([a assets])
                ([l (nested-labels label)])
        (hash-ref a l)))
    (if (box? value)
        (unbox value)
        value))

  (define (compile-assets)
    ;; Compiling assets is done in multiple passes because some assets depend
    ;; on other assets. Alternatively, one could construct a dependency graph
    ;; and then compile assets in the right order. The following code stores
    ;; the values of assets in boxes that start out undefined. When an asset
    ;; can be compiled, its value is put into the box. Compilation iterates
    ;; until all bpxes have defined values.
    (define unevaluated empty)
    ;; Helper function for compiling a single asset.
    (define (compile-asset decl)
      (match decl
        [(list 'rule arg ...)
         (box (compile-rule signature decl #f))]
        [(list 'equation arg ...)
         (box (compile-equation signature decl))]
        [(list 'transformation arg ...)
         (box (compile-transformation signature decl))]
        [(or (list 'as-equation _)
             (list 'as-rule _ _)
             (list 'substitute _ _ _)
             (list 'transform _ _ _))
         (let ([no-value (box #f)])
           (set! unevaluated (cons (list no-value decl) unevaluated))
           no-value)]
        [(list 'assets assets)
         (for/hash ([(label value) assets])
           (values label (compile-asset value)))]
        [term
         (box ((compile-term signature) decl))]))
    ;; Helper functions for computing dependent assets
    (define (compute-asset assets decl)
      (match decl
        [(list 'as-equation label)
         (and~> (lookup-asset assets label)
                (as-equation* signature _))]
        [(list 'as-rule label flip?)
         (and~> (lookup-asset assets label)
                (as-rule* signature _ flip?))]
        [(list 'substitute rule-label asset-label reduce?)
         (let* ([rule (lookup-asset assets rule-label)]
                [asset (lookup-asset assets asset-label)]
                [substituted (and rule
                                  asset
                                  (substitution* signature rule asset))])
           (if reduce?
               (and substituted (reduce* signature rules substituted))
               substituted))]
        [(list 'transform tr-label asset-label reduce?)
         (let* ([tr (lookup-asset assets tr-label)]
                [asset (lookup-asset assets asset-label)]
                [transformed (and tr
                                  asset
                                  (transformation* signature tr asset))])
           (if reduce?
               (and transformed (reduce* signature rules transformed))
               transformed))]))

    (define (compute-assets unevaluated)
      (for/fold ([remaining empty])
                ([box+decl unevaluated])
        (match-define (list box decl) box+decl)
        (define value (compute-asset assets decl))
        (if value
            (begin (set-box! box value)
                   remaining)
            (cons (list box decl) remaining))))
    (define (compute-assets-iteratively unevaluated)
      (let ([remaining (compute-assets unevaluated)])
        (if (equal? (length remaining) (length unevaluated))
            remaining
            (compute-assets-iteratively remaining))))
    (define (combine-compiled-assets label asset1 asset2)
      (cond
        [(equal? asset1 asset2)
         asset1]
        [(and (hash? asset1) (hash? asset2))
         (hash-union asset1 asset2 #:combine/key combine-compiled-assets)]
        [(and (box? asset1) (not (unbox asset1)))
         asset2]
        [(and (box? asset2) (not (unbox asset2)))
         asset1]
        [else
         (error (format "Asset label ~a already used for value ~a" label asset1))]))
    ;; Merge the assets of the included contexts.
    (define after-includes
      (for/fold ([merged-assets (hash)])
                ([m/c includes])
        (hash-union merged-assets (context-compiled-assets (cdr m/c))
                    #:combine/key combine-compiled-assets)))
    ;; Process the asset declarations
    (define assets
      (for/fold ([assets after-includes])
                ([(label ad) (context-assets cntxt)])
        (with-handlers ([exn:fail? (re-raise-exn `(asset ,label ,ad))])
          (hash-union assets (hash label (compile-asset ad))
                      #:combine/key combine-compiled-assets))))
    ;; Compute unevaluated assets
    (define remaining (compute-assets-iteratively unevaluated))
    (unless (empty? remaining)
      (error (format "~a remaining unevaluated assets" (length remaining))))
    ;; Return compiled assets, guaranteed without unevaluated boxes
    assets)

  (define assets (compile-assets))

  (struct-copy context cntxt
               (compiled-signature signature)
               (compiled-rules     rules)
               (compiled-assets    assets)))

(define (without-compiled-resources cntxt)
  (struct-copy context cntxt
               (compiled-signature #f)
               (compiled-rules     #f)
               (compiled-assets    #f)))

(module+ test
 
  (define compiled-reference-context (compile-context reference-context
                                                      dummy-name-resolver))
  (let* ([c compiled-reference-context]
         [signature (context-compiled-signature c)]
         [sort-graph (operators:signature-sort-graph signature)]
         [rules (context-compiled-rules c)]
         [assets (context-compiled-assets c)])
    (check-equal? (sorts:all-sorts sort-graph) (context-sorts c))
    (check-equal? (sorts:all-subsort-relations sort-graph) (context-subsorts c))
    (check-equal? (for/sum ([(symbol arity sort) (operators:all-ops signature)]) 1)
                  (set-count (context-ops c)))
    (check-equal? (operators:all-vars signature)
                  (context-vars c))
    (check-equal? (length (sequence->list (equations:in-rules rules)))
                  (length (context-rules c)))
    (check-equal? (hash-count assets)
                  (hash-count (context-assets c))))

  (check-equal? (without-compiled-resources compiled-reference-context)
                reference-context))

;;
;; Builtin contexts
;;

(define (make-builtin-context includes signature rules)
  (struct-copy context empty-context
               (includes           includes)
               (compiled-signature signature)
               (compiled-rules     rules)
               (compiled-assets    (hash))))

;;
;; Check and evaluate assets within a context
;;

(define (check-asset cntxt xexpr-asset)
  (define signature (context-compiled-signature cntxt))
  (unless signature
    (error "check-asset requires a compiled context"))
  (define asset (xexpr->asset xexpr-asset))
  (match asset
    [(list (and tag
                (or 'rule 'equation 'transformation 'assets 'test
                    'as-equation 'as-rule 'substitute 'transform)) _ ...)
     (error (format "not yet implemented:" tag))]
    [term
     ((compile-term signature) asset)]))

(module+ test
  (check-not-false (check-asset compiled-reference-context
                                '(term-or-var ((name "a-foo")))))
  (check-not-false (check-asset compiled-reference-context
                                '(term ((op "a-foo")) (term-or-var ((name "a-foo"))))))
  (check-exn exn:fail?
             (thunk (check-asset reference-context
                                 '(term-or-var ((name "a-foo"))))))
  (check-exn exn:fail?
             (thunk (check-asset compiled-reference-context
                                 '(term-or-var ((name "an-undefined-op")))))))

(define (uncompile-term compiled-term)
  (define-values (op args) (terms:term.op-and-args compiled-term))
  (cond
    [op
     (list 'term op (map uncompile-term args))]
    [(terms:var? compiled-term)
     (list 'var (terms:var-name compiled-term) (terms:var-sort compiled-term))]
    [(integer? compiled-term)
     (list 'integer compiled-term)]
    [(rational? compiled-term)
     (list 'rational compiled-term)]
    [(inexact? compiled-term)
     (list 'floating-point compiled-term)]
    [else
     (error "illegal term type")]))

(define (eval-asset cntxt xexpr-asset)
  (define signature (context-compiled-signature cntxt))
  (unless signature
    (error "eval-asset requires a compiled context"))
  (define rules (context-compiled-rules cntxt))
  (define asset (xexpr->asset xexpr-asset))
  (match asset
    [(list (and tag
                (or 'rule 'equation 'transformation 'assets
                    'as-equation 'as-rule 'substitute 'transform)) _ ...)
     (error (format "asset of type ~a cannot be evaluated" tag))]
    [(list 'test term reduced-term)
     (define make (compile-term signature))
     (define term* (make term))
     (define reduced-term* (make reduced-term))
     (define actual-reduced-term* (rewrite:reduce signature rules term*))
     (list 'test-result
           (uncompile-term term*)
           (uncompile-term reduced-term*)
           (uncompile-term actual-reduced-term*)
           (equal? reduced-term* actual-reduced-term*))]
    [term
     (define term* ((compile-term signature) term))
     (define reduced-term* (rewrite:reduce signature rules term*))
     (uncompile-term reduced-term*)]))

(module+ test
  ;; a successful test
  (check-equal?
   (eval-asset compiled-reference-context
               '(test (term (term ((op "foo2bar"))
                                  (term-or-var ((name "a-bar")))))
                      (reduced-term (term-or-var ((name "a-foo"))))))
   (let ([t '(term foo2bar ((term a-bar ())))]
         [rt '(term a-foo ())])
     (list 'test-result t rt rt #t)))
  ;; a failing test
  (check-equal?
   (eval-asset compiled-reference-context
               '(test (term (term-or-var ((name "a-foo"))))
                      (reduced-term (term-or-var ((name "a-bar"))))))
   (let ([t '(term a-foo ())]
         [rt '(term a-bar ())])
     (list 'test-result t rt t #f)))
  ;; a term
  (check-equal? (eval-asset compiled-reference-context
                            '(term ((op "foo2bar"))
                                   (term-or-var ((name "a-bar")))))
                '(term a-foo ()))
  ;; failure for an asset list
  (check-exn exn:fail?
             (thunk (eval-asset compiled-reference-context
                                '(assets)))))
