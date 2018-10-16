#lang racket

(provide
 (struct-out exn:fail:leibniz)
 (struct-out context)
 (struct-out compiled-context)
 re-raise-exn
 (contract-out
  [empty-context context?]
  [context-name (context? . -> . (or/c #f string?))]
  [context-document (context? . -> . (or/c #f string?))]
  [xexpr->context (xexpr/c (or/c #f string?) . -> . context?)]
  [context->xexpr ((context?) (string?) . ->* . xexpr/c)]
  [add-implicit-declarations (context? . -> . context?)]
  [compile-context (context? (string? (or/c #f string?) symbol?
                             . -> . (or/c context?
                                          (list/c (or/c string? #f) string?)))
                   . -> . compiled-context?)]
  [without-compiled-resources (context? . -> . context?)]
  [make-builtin-context
         ((listof pair?) operators:signature? equations:rulelist?
         . -> . compiled-context?)]
  [check-asset (compiled-context? xexpr/c . -> . any/c)]
  [eval-asset (compiled-context? xexpr/c . -> . any/c)]
  [eval-context-expr (compiled-context? xexpr/c . -> . context?)]))

(require (prefix-in sorts: "./sorts.rkt")
         (prefix-in operators: "./operators.rkt")
         (prefix-in terms: "./terms.rkt")
         (prefix-in equations: "./equations.rkt")
         (prefix-in rewrite: "./rewrite.rkt")
         (only-in racket/hash hash-union)
         txexpr
         xml
         threading)

(module+ test
  (require rackunit
           racket/function))

;;
;; A context is the main unit of Leibniz code. It defines a term algebra
;; (sort graph, operators, variables) and a list of rewrite rules,
;; plus optional names values (terms, equations, or rules) called assets
;;

(struct context (includes context-refs sorts subsorts vars ops rules assets
                 origin)
  #:transparent
  ;; includes: a list of include declarations
  ;; context-refs: a hash mapping op names to context references
  ;; sorts: a set of declared sorts
  ;; subsorts: a set of subsort declarations (pairs of sorts)
  ;; vars: a hash mapping var names to sorts
  ;; ops: a set of op declarations (sexps)
  ;; rules: a list of rules (sexps)
  ;; assets: a hash mapping labels to assets (sexps)
  ;; origin: a (document . name) pair, where document is the sha256 hash
  ;;         (hex string) of the HTML document in which the context is
  ;;         defined, and name is the local name of the context in that
  ;;         document. Eith item can be #f if the information is not available.
  #:methods terms:gen:term
  [(define (term.sort c) 'context)
   (define (term.builtin-type c) '*context*)
   (define term.key term.builtin-type)]
  ;; #:methods gen:custom-write
  ;; [(define (write-proc term port [mode #f])
  ;;    (display "<struct context>" port))]
)

(struct compiled-context context (compiled-signature compiled-rules compiled-assets)
  #:transparent
  ; compiled-signature: optimized representation of sorts, subsorts, vars, and ops
  ; compiled-rules: optimized representation of rules
  ; compiled-assets: optimized representation of assets
)

(define (context-name cntxt)
  (cdr (context-origin cntxt)))

(define (context-document cntxt)
  (car (context-origin cntxt)))

(define empty-context
  (context empty (hash) (set) (set)  (hash) (set) (list) (hash) (cons #f #f)))

(module+ test

  (define (dummy-name-resolver path doc-sha256 request-type)
    (error "Call of dummy function"))

  (define xexpr-context
    '(context ((id "test"))
              (includes)
              (context-refs)
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
                                                    (term-or-var ((name "a-foo")))))))))))

  (define reference-context
    (context empty
             (hash)
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
             (cons #f "test"))))

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
             [`(condition () ... ,term) (xexpr->asset term)]))]
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
             [`(condition () ... ,term) (xexpr->asset term)]))]
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
             [`(condition () ... ,term) (xexpr->asset term)]))]
    [`(term ((op ,op-string)) ,args ...)
     (list 'term
           (string->symbol op-string)
           (map xexpr->asset args))]
    [`(term-or-var ((name ,name-string)))
     (list 'term-or-var (string->symbol name-string))]
    [`(string ((value ,s)))
     (list 'string s)]
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
     (list 'test (xexpr->asset t) (xexpr->asset rt))]
    [`(trace () ... ,t)
     (list 'trace (xexpr->asset t))]
    [else
     (error (format "Unknown xexpr type: ~a" xexpr-asset))]))

(define (xexpr->context xexpr-context origin-sha256)

  (match-define
    `(context (,attrs ...) ...
              (includes () ... ,xexpr-includes ...)
              (context-refs () ... ,xexpr-context-refs ...)
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
  (define includes (for/list ([inc xexpr-includes])
                     (define attrs (attrs->hash (get-attrs inc)))
                     (list (string->symbol (hash-ref attrs 'mode))
                           (hash-ref attrs 'context)
                           (hash-ref attrs 'document origin-sha256))))
  (define context-refs (for/hash ([cref xexpr-context-refs])
                         (define attrs (attrs->hash (get-attrs cref)))
                         (values (string->symbol (hash-ref attrs 'op))
                                 (cons (hash-ref attrs 'context)
                                       (hash-ref attrs 'document origin-sha256)))))
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

  (context includes context-refs sorts subsorts vars ops rules assets
           (cons origin-sha256 name)))

(module+ test

  (check-equal? (xexpr->context '(context (includes)
                                          (context-refs)
                                          (sorts)
                                          (subsorts)
                                          (vars)
                                          (ops)
                                          (rules)
                                          (assets))
                                #f)
                empty-context)

  (check-equal? (xexpr->context xexpr-context #f)
                reference-context)

  (check-equal? reference-context
                (~> xexpr-context
                    xexpr->string
                    string->xexpr
                    (xexpr->context #f))))

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
      [(list 'string s)
       `(string ((value ,s)))]
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
            (includes ,@(for/list ([inc (context-includes cntxt)])
                          (match-define (list mode c-name document) inc)
                          (define attrs `((mode ,(symbol->string mode))
                                          (context ,c-name)))
                          `(include ,(if document
                                         (cons `(document ,document) attrs)
                                         attrs))))
            (context-refs ,@(for/list ([(op ref) (context-context-refs cntxt)])
                              (match-define (cons c-name document) ref)
                              (define attrs `((op ,(symbol->string op))
                                              (context ,c-name)))
                              `(context-ref ,(if document
                                                 (cons `(document ,document)
                                                       attrs)
                                                 attrs))))
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
  (check-equal? reference-context
                (~> reference-context
                    (context->xexpr "test")
                    (xexpr->context #f))))

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

  (define ops
    (set-union ; the original op declarations
               (context-ops cntxt)
               ; the names from context refs, with sort context
               (for/set ([(name cref) (context-context-refs cntxt)])
                 (list name empty 'context))))

  (context (context-includes cntxt)
           (context-context-refs cntxt)
           sorts
           (context-subsorts cntxt)
           vars
           ops
           (context-rules cntxt)
           (context-assets cntxt)
           (context-origin cntxt)))

(module+ test

  (check-equal? (add-implicit-declarations reference-context)
                reference-context)

  (define a-full-context
    (context empty
             (hash)
             (set 'foo 'bar 'baz)
             (set (cons 'foo 'bar))
             (hash 'X 'bar)
             (set '(a-foo () foo)
                  '(a-bar () bar)
                  '(a-baz () baz)
                  '(bar2foo ((var X bar)) foo))
             empty
             (hash)
             (cons #f #f)))

  (define a-minimal-context
    (context empty
             (hash)
             (set)
             (set (cons 'foo 'bar))
             (hash)
             (set '(a-foo () foo)
                  '(a-bar () bar)
                  '(a-baz () baz)
                  '(bar2foo ((var X bar)) foo))
             empty
             (hash)
             (cons #f #f)))

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
                 [(list (or 'integer 'rational 'floating-point 'string) x) x])])
    fn))

(define (compile-pattern signature local-vars)
  (letrec ([fn (match-lambda
                 [#f
                  #f]
                 [(list 'term-or-var name)
                  (terms:make-var-or-term signature name local-vars)]
                 [(list 'term op args)
                  (terms:make-term signature op (map fn args))]
                 [(list (or 'integer 'rational 'floating-point 'string) x) x])])
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
    (for/list ([inc (context-includes cntxt)])
      (match-define (list mode c-name document) inc)
      (cons mode
            (with-handlers ([exn:fail? (re-raise-exn (list mode c-name document))])
              (name-resolver c-name document 'context)))))

  (define (compile-sort-graph)
    ;; Merge the sort graphs of the included contexts.
    (define after-includes
      (for/fold ([ms sorts:empty-sort-graph])
                ([m/c includes])
        (sorts:merge-sort-graphs ms
                                 (operators:signature-sort-graph
                                  (compiled-context-compiled-signature (cdr m/c))))))
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
        (define csig (compiled-context-compiled-signature (cdr m/c)))
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
                                   (compiled-context-compiled-rules (cdr m/c))
                                   signature)))
    ;; Process the context-ref declarations
    (define after-context-refs
      (for/fold ([rl after-includes])
                ([(name cref) (context-context-refs cntxt)])
        (match-define (cons c-name document) cref)
        (define c-term
          (terms:make-term signature 'context
                           (if document
                               (list document c-name)
                               (list c-name))))
        (equations:add-rule
         rl
         (equations:make-rule signature
                              (terms:make-term signature name empty)
                              #f
                              c-term
                              #f #t))))
    ;; Process the rule declarations
    (for/fold ([rl after-context-refs])
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
        (hash-union merged-assets (compiled-context-compiled-assets (cdr m/c))
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

  (compiled-context (context-includes cntxt)
                    (context-context-refs cntxt)
                    (context-sorts cntxt)
                    (context-subsorts cntxt)
                    (context-vars cntxt)
                    (context-ops cntxt)
                    (context-rules cntxt)
                    (context-assets cntxt)
                    (context-origin cntxt)
                    signature
                    rules
                    assets))

(define (without-compiled-resources cntxt)
  (struct-copy context cntxt))

(module+ test
 
  (define compiled-reference-context (compile-context reference-context
                                                      dummy-name-resolver))
  (let* ([c compiled-reference-context]
         [signature (compiled-context-compiled-signature c)]
         [sort-graph (operators:signature-sort-graph signature)]
         [rules (compiled-context-compiled-rules c)]
         [assets (compiled-context-compiled-assets c)])
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
    (compiled-context includes
                      (context-context-refs empty-context)
                      (context-sorts empty-context)
                      (context-subsorts empty-context)
                      (context-vars empty-context)
                      (context-ops empty-context)
                      (context-rules empty-context)
                      (context-assets empty-context)
                      (context-origin empty-context)
                      signature
                      rules
                      (hash)))

;;
;; Check and evaluate assets within a context
;;

(define (decompile-pattern compiled-term)
  (define-values (op args) (terms:term.op-and-args compiled-term))
  (cond
    [op
     (list 'term op (map decompile-pattern args))]
    [(terms:var? compiled-term)
     (list 'var (terms:var-name compiled-term) (terms:var-sort compiled-term))]
    [(integer? compiled-term)
     (list 'integer compiled-term)]
    [(rational? compiled-term)
     (list 'rational compiled-term)]
    [(inexact-real? compiled-term)
     (list 'floating-point compiled-term)]
    [(string? compiled-term)
     (list 'string compiled-term)]
    [(context? compiled-term)
     (list 'context (context-name compiled-term) (context-document compiled-term))]
    [else
     (error (format "cannot decompile term: ~v" compiled-term))]))

(define (decompile-item compiled-item)
  (cond
    [(list? compiled-item)
     (map decompile-item compiled-item)]
    [(equations:rule? compiled-item)
     (define replacement (equations:rule-replacement compiled-item))
     (define decompiled-replacement
       (if (procedure? replacement)
           'procedure
           (decompile-pattern replacement)))
     (list 'rule
           (for/hash ([v (terms:term.vars (equations:rule-pattern compiled-item))])
             (values (terms:var-name v) (terms:var-sort v)))
           (decompile-pattern (equations:rule-pattern compiled-item))
           decompiled-replacement
           (and (equations:rule-condition compiled-item)
                (decompile-pattern (equations:rule-condition compiled-item))))]
    [(terms:term? compiled-item)
     (decompile-pattern compiled-item)]
    [(terms:substitution? compiled-item)
     (for/hash ([(name value) compiled-item])
       (values name (decompile-pattern value)))]
    ;; TODO should test for all possible decompilable item types
    [else
     compiled-item]))

(define (check-asset cntxt xexpr-asset)
  (define signature (compiled-context-compiled-signature cntxt))
  (define asset (xexpr->asset xexpr-asset))
  (match asset
    [(list (and tag
                (or 'transformation 'assets 'test
                    'as-equation 'as-rule 'substitute 'transform)) _ ...)
     (error (format "not yet implemented: ~a" tag))]
    [(list (and type (or 'rule 'equation)) vars pattern1 pattern2 condition)
     (define make (compile-pattern signature vars))
     (define pattern1* (make pattern1))
     (define pattern2* (make pattern2))
     (define condition* (and condition (make condition)))
     (list type vars
           (decompile-pattern pattern1*)
           (decompile-pattern pattern2*)
           (and condition (decompile-pattern condition*)))]
    [pattern
     (define pattern* ((compile-pattern signature (hash)) asset))
     (decompile-pattern pattern*)]))

(module+ test
  (check-equal? (check-asset compiled-reference-context
                             '(term-or-var ((name "a-foo"))))
                '(term a-foo ()))
  (check-equal? (check-asset compiled-reference-context
                             '(term ((op "a-foo")) (term-or-var ((name "a-foo")))))
                '(term a-foo ((term a-foo ()))))
  (check-exn exn:fail?
             (thunk (check-asset reference-context
                                 '(term-or-var ((name "a-foo"))))))
  (check-exn exn:fail?
             (thunk (check-asset compiled-reference-context
                                 '(term-or-var ((name "an-undefined-op")))))))

(define (eval-asset cntxt xexpr-asset)
  (define signature (compiled-context-compiled-signature cntxt))
  (define rules (compiled-context-compiled-rules cntxt))
  (define asset (xexpr->asset xexpr-asset))
  (match asset
    [(list (and tag
                (or 'equation 'transformation 'assets
                    'as-equation 'as-rule 'substitute 'transform)) _ ...)
     (error (format "asset of type ~a cannot be evaluated" tag))]
    [(list 'test term reduced-term)
     (define make (compile-term signature))
     (define term* (make term))
     (define reduced-term* (make reduced-term))
     (define actual-reduced-term* (rewrite:reduce signature rules term*))
     (list 'test-result
           (decompile-pattern term*)
           (decompile-pattern reduced-term*)
           (decompile-pattern actual-reduced-term*)
           (equal? reduced-term* actual-reduced-term*))]
    [(list 'trace term)
     (define term* ((compile-term signature) term))
     (define logger (rewrite:trace-logger))
     (define trace* (rewrite:trace-reduce signature rules term* logger))
     (list 'trace
           (for/list ([record (logger 'get-value)])
             (list* (first record) (second record)
                    (map decompile-item (drop record 2)))))]
    [term
     (define term* ((compile-term signature) term))
     (define reduced-term* (rewrite:reduce signature rules term*))
     (list 'reduced-term (decompile-pattern reduced-term*))]))

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
                '(reduced-term (term a-foo ())))
  ;; failure for an asset list
  (check-exn exn:fail?
             (thunk (eval-asset compiled-reference-context
                                '(assets)))))

;;
;; Evaluate an expression yielding a context
;;
(define (eval-context-expr cntxt xexpr)
  (define signature (compiled-context-compiled-signature cntxt))
  (define rules (compiled-context-compiled-rules cntxt))
  (define term (xexpr->asset xexpr))
  (define term* ((compile-term signature) term))
  (define reduced-term* (rewrite:reduce signature rules term*))
  (unless (context? reduced-term*)
    (raise (exn:fail:leibniz
            (format "does not reduce to a context structure: ~a"
                    reduced-term*)
            (current-continuation-marks)
            xexpr)))
  ;; This function is used only in substitute-context, so the origin
  ;; of the evaluated context is the defining context's origin.
  (struct-copy context reduced-term*
               [origin (context-origin cntxt)]))
