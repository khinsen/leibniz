#lang racket

(provide empty-document
         add-library add-context
         new-context-from-declarations new-context-from-source
         get-context get-context-declarations
         get-document-sxml get-context-sxml
         make-term make-rule make-equation
         make-test
         write-signature-graphs)

(require (prefix-in sorts: "./sorts.rkt")
         (only-in "./sorts.rkt" sort-graph? empty-sort-graph)
         (prefix-in operators: "./operators.rkt")
         (prefix-in terms: "./terms.rkt")
         (prefix-in equations: "./equations.rkt")
         (prefix-in contexts: "./contexts.rkt")
         (prefix-in rewrite: "./rewrite.rkt")
         (prefix-in tools: "./tools.rkt")
         "./lightweight-class.rkt"
         racket/hash
         threading)

(module+ test
  (require rackunit)

  (contexts:define-context test-context1
    (sort foo)
    (sort bar)
    (subsort foo bar))
  
  (contexts:define-context test-context2
    (sort foo)
    (sort bar)
    (sort boolean)
    (subsort foo bar)
    (op a-foo foo)
    (op (a-foo bar) foo)
    (op (a-bar foo) bar)
    (op true boolean)
    (op false boolean)
    (op (_∧ boolean boolean) boolean)
    (op (a-test foo) boolean)
    (op (another-test foo) boolean)
    (=> #:var (X foo) (a-foo X) a-foo #:if (_∧ (a-test X) (another-test X)))
    (eq a-foo (a-foo a-foo))))

; Re-raise exceptions with the source location information from the document

(define-struct (exn:fail:leibniz exn:fail) (a-srcloc)
  #:property prop:exn:srclocs
  (λ (a-struct)
    (match a-struct
      [(struct exn:fail:leibniz (msg marks a-srcloc))
       (list a-srcloc)])))

(define ((re-raise-exn loc) e)
  (if loc 
      (raise (make-exn:fail:leibniz
              (exn-message e)
              (current-continuation-marks)
              (apply srcloc loc)))
      (raise e)))

; Make a signature from a sequence of declarations

(define (get-loc locs decl)
  (cond
    [(procedure? locs)
     (locs decl)]
    [(hash? locs)
     (hash-ref locs decl #f)]
    [else
     #f]))

(define (make-sort-graph includes sort-decls subsort-decls locs)
  (define after-includes
    (for/fold ([ms sorts:empty-sort-graph])
              ([c includes])
      (sorts:merge-sort-graphs ms (contexts:context-sort-graph c))))
  (define after-sorts
    (for/fold ([sorts after-includes])
              ([s sort-decls])
      (with-handlers ([exn:fail? (re-raise-exn (get-loc locs s))])
        (sorts:add-sort sorts s))))
  (for/fold ([sorts after-sorts])
            ([ss subsort-decls])
    (with-handlers ([exn:fail? (re-raise-exn (get-loc locs ss))])
      (sorts:add-subsort-relation sorts (car ss) (cdr ss)))))

(define (make-signature sorts includes op-decls locs)
  (define (argsort sort-or-var-decl)
    (match sort-or-var-decl
      [(list 'sort sort-id) sort-id]
      [(list 'var var-name sort-id) sort-id]))
  (define after-includes
    (for/fold ([msig (operators:empty-signature sorts)])
              ([c includes])
      (operators:merge-signatures msig
                                  (contexts:context-signature c)
                                  sorts)))
  (define signature
    (for/fold ([sig after-includes])
              ([od op-decls])
      (with-handlers ([exn:fail? (re-raise-exn (get-loc locs od))])
        (match-define (list name arity rsort) od)
        (operators:add-op sig name (map argsort arity) rsort
                          #:meta (get-loc locs od)))))
  (define non-regular (operators:non-regular-op-example signature))
  (when non-regular
    (match-let ([(list op arity rsorts) non-regular])
      (displayln (format "Warning: operator ~a~a has ambiguous sorts ~a"
                         op arity rsorts))))
  signature)

(define (make-varset signature includes var-decls locs)
  (define sorts (operators:signature-sort-graph signature))
  (define after-includes
    (for/fold ([mv (terms:empty-varset sorts)])
              ([c includes])
      (terms:merge-varsets mv (contexts:context-vars c) sorts)))
  (for/fold ([vs after-includes])
            ([vd var-decls])
    (with-handlers ([exn:fail? (re-raise-exn (get-loc locs vd))])
      (match-define (list 'var name sort) vd)
      (terms:add-var vs name sort))))

(define (make-term* signature varset)
  (letrec ([fn (match-lambda
                 [(list 'term name '())
                  (terms:make-var-or-term signature varset name)]
                 [(list 'term op args)
                  (terms:make-term signature op (map fn args))]
                 [(list 'integer n) n]
                 [(list 'rational r) r]
                 [(list 'floating-point fp) fp])])
    fn))

(define (make-local-varset* signature varset decls)
  (define sorts (operators:signature-sort-graph signature))
  (for/fold ([vs varset])
            ([decl decls])
    (match decl
      [(list 'var name sort)
       (terms:add-var vs name sort)]
      [_ vs])))

(define (make-pattern* signature varset)
  (letrec ([fn (match-lambda
                 [#f
                  #f]
                 [(list 'term name '())
                  (terms:make-var-or-term signature varset name)]
                 [(list 'term op args)
                  (terms:make-term signature op (map fn args))]
                 [(list 'integer n) n]
                 [(list 'rational r) r]
                 [(list 'floating-point fp) fp])])
    fn))

(define (make-condition* clauses)
  (for/fold ([condition #f])
              ([c clauses])
      (match c
        [(list 'var name sort)
         condition]
        [term
         (if condition
             (list 'term '_∧ (list condition term))
             term)])))

(define (make-rule* signature varset rule-expr)
  (match rule-expr
    [(list 'rule pattern replacement clauses)
     (let* ([varset (make-local-varset* signature varset clauses)]
            [mp (make-pattern* signature varset)])
       (equations:make-rule signature (mp pattern)
                            (mp (make-condition* clauses))
                            (mp replacement)
                            #f #t))]
    [_ #f]))

(define (make-rulelist signature varset includes rule-decls locs)
  (define after-includes
    (for/fold ([mrl equations:empty-rulelist])
              ([c includes])
      (equations:merge-rulelists mrl (contexts:context-rules c) signature)))
  (for/fold ([rl after-includes])
            ([rd rule-decls])
    (with-handlers ([exn:fail? (re-raise-exn (get-loc locs rd))])
      (equations:add-rule rl (make-rule* signature varset rd)))))

(define (make-equation* signature varset equation-expr)
  (match equation-expr
    [(list 'equation left right clauses)
     (let* ([varset (make-local-varset* signature varset clauses)]
            [mp (make-pattern* signature varset)])
       (equations:make-equation signature
                                (mp left)
                                (mp (make-condition* clauses))
                                (mp right)
                                #f))]
    [_ #f]))

(define (make-equationset signature varset includes eq-decls locs)
    (define after-includes
    (for/fold ([mes equations:empty-equationset])
              ([c includes])
      (equations:merge-equationsets mes (contexts:context-equations c) signature)))
  (for/fold ([eq after-includes])
            ([ed eq-decls])
    (with-handlers ([exn:fail? (re-raise-exn (get-loc locs ed))])
      (equations:add-equation eq (make-equation* signature varset ed)))))


; Generate a list of declarations that extends base-context to full-context

(define (sort-graph-diff base-sort-graph full-sort-graph)
  (hash 'sorts (set-subtract (sorts:all-sorts full-sort-graph)
                             (sorts:all-sorts base-sort-graph))
        'subsorts (set-subtract (sorts:all-subsort-relations full-sort-graph)
                                (sorts:all-subsort-relations base-sort-graph))))

(define (op-diff base-signature full-signature)
  (define base-ops
    (for/set ([(symbol rank meta) (operators:all-ops base-signature)])
      (cons symbol rank)))
  (hash 'ops
        (for/set ([(symbol rank meta) (operators:all-ops full-signature)]
                  #:unless (set-member? base-ops (cons symbol rank)))
          (list symbol (map (λ (s) `(sort ,s)) (car rank)) (cdr rank)))))

(define (varset-diff base-varset full-varset)
  (define base-vars (terms:all-vars base-varset))
  (hash 'vars
        (for/set ([(name sort) (in-hash (terms:all-vars full-varset))]
                  #:unless (hash-has-key? base-vars name))
          (list 'var name sort))))

(define (term->decl term)
  (define-values (op args) (terms:term.op-and-args term))
  (cond
    [op
     (list 'term op (map term->decl args))]
    [(terms:var? term)
     (list 'term (terms:var-name term) empty)]
    [(integer? term)
     (list 'integer term)]
    [(and (number? term) (exact? term))
     (list 'rational term)]
    [(flonum? term)
     (list 'floating-point term)]
    [else
     (error (format "illegal term ~a" term))]))

(define (rule->decl rule)
  (define pattern (equations:rule-pattern rule))
  (define replacement (equations:rule-replacement rule))
  (define condition (equations:rule-condition rule))
  (define vars (terms:term.vars pattern))
  (define decl-pattern(term->decl pattern))
  (define decl-replacement (if (procedure? replacement)
                               replacement
                               (term->decl replacement)))
  (define var-clauses
    (for/list ([var (in-set vars)])
      (list 'var (terms:var-name var) (terms:var-sort var))))
  (define decl-clauses
    (if condition
        (cons (term->decl condition) var-clauses)
        var-clauses))
  (list 'rule decl-pattern decl-replacement decl-clauses))

(define (eq->decl rule)
  (define left (equations:equation-left rule))
  (define right (equations:equation-right rule))
  (define condition (equations:equation-condition rule))
  (define vars (set-union (terms:term.vars left) (terms:term.vars right)))
  (define decl-left (term->decl left))
  (define decl-right (term->decl right))
  (define var-clauses
    (for/list ([var (in-set vars)])
      (list 'var (terms:var-name var) (terms:var-sort var))))
  (define decl-clauses
    (if condition
        (cons (term->decl condition) var-clauses)
        var-clauses))
  (list 'equation decl-left decl-right decl-clauses))

(define (rulelist-diff base-rulelist full-rulelist)
  (define base-rules (for/set ([rule (equations:in-rules base-rulelist)])
                       (rule->decl rule)))
  (define full-rules (for/list ([rule (equations:in-rules full-rulelist)])
                       (rule->decl rule)))
  (hash 'rules
        (filter (λ (r) (not (set-member? base-rules r))) full-rules)))

(define (equationset-diff base-equationset full-equationset)
  (define base-equations (for/set ([eq (equations:in-equations base-equationset)])
                           (eq->decl eq)))
  (define full-equations (for/list ([eq (equations:in-equations full-equationset)])
                           (eq->decl eq)))
  (hash 'equations
        (filter (λ (eq) (not (set-member? base-equations eq))) full-equations)))

(define (context-diff base-context full-context)
  (define base-signature (contexts:context-signature base-context))
  (define full-signature (contexts:context-signature full-context))
  (define base-sort-graph (operators:signature-sort-graph base-signature))
  (define full-sort-graph (operators:signature-sort-graph full-signature))
  (define base-varset (contexts:context-vars base-context))
  (define full-varset (contexts:context-vars full-context))
  (hash-union (sort-graph-diff base-sort-graph full-sort-graph)
              (op-diff base-signature full-signature)
              (varset-diff base-varset full-varset)
              (rulelist-diff (contexts:context-rules base-context)
                             (contexts:context-rules full-context))
              (equationset-diff (contexts:context-equations base-context)
                                (contexts:context-equations full-context))))

; Documents track declarations and expressions embedded in a Scribble document

(define-class document

  (field contexts decls library)
  ; contexts: a hash mapping context names (strings) to contexts
  ; decls: a hash mapping context names to hashes with keys
  ;        'includes 'sorts 'ops 'vars 'rules 'equations,
  ;        values are lists of the declarations in each category
  ; library: a hash mapping document names to documents

  (define (add-library name library-document)
    (document contexts
              decls
              (hash-set library name library-document)))

  (define (add-context name include-refs context)
    (define temp-doc (new-context-from-source name include-refs empty))
    (define inclusion-context (send temp-doc get-context name))
    (define full-context
      (contexts:merge-contexts inclusion-context context))
    (define added-decls (context-diff inclusion-context full-context))
    (document (hash-set contexts name full-context)
              (hash-set decls name
                        (hash-set added-decls 'includes (map car include-refs)))
              library))

  (define (new-context-from-source name include-refs context-decls)

    (define (add-loc decls decl loc)
      (hash-update decls 'locs
                   (λ (ls)
                     (if (and loc (not (hash-has-key? ls decl)))
                         (hash-set ls decl loc)
                         ls))))

    (define (add-sort decls s loc)
      (~> decls
          (hash-update 'sorts (λ (ss) (set-add ss s)))
          (add-loc s loc)))

    (define (add-subsort decls s1 s2 loc)
      (define new-ss (cons s1 s2))
      (~> decls
          (hash-update 'subsorts (λ (ss) (set-add ss new-ss)))
          (add-loc new-ss loc)))

    (define (add-op decls name arity rsort loc)
      (define new-op (list name arity rsort))
      (~> decls
          (hash-update 'ops (λ (ops) (set-add ops new-op)))
          (add-loc new-op loc)))

    (define (add-var decls name sort loc)
      (define new-var (list 'var name sort))
      (~> decls
          (hash-update 'vars (λ (vars) (set-add vars new-var)))
          (add-loc new-var loc)))

    (define (add-rule decls pattern replacement clauses loc)
      (define new-rule (list 'rule pattern replacement clauses))
      (~> decls
          (hash-update 'rules (λ (rules) (if (member new-rule rules)
                                             rules
                                             (cons new-rule rules))))
          (add-loc new-rule loc)))

    (define (add-equation decls left right clauses loc)
      (define new-eq (list 'equation left right clauses))
      (~> decls
          (hash-update 'equations (λ (eqs) (set-add eqs new-eq)))
          (add-loc new-eq loc)))

    (define decls
      (~> (for/fold ([decls (hash 'includes (map car include-refs)
                                  'sorts (set)
                                  'subsorts (set)
                                  'ops (set)
                                  'vars (set)
                                  'rules (list)
                                  'equations (set)
                                  'locs (for/hash ([name/loc include-refs])
                                          (values (car name/loc) (cdr name/loc))))])
                    ([decl/loc context-decls])
            (match-define (cons decl loc) decl/loc)
            (match decl
              [(list 'sort s)
               (add-sort decls s loc)]
              [(list 'subsort s1 s2)
               (~> decls
                   (add-sort s1 loc)
                   (add-sort s2 loc)
                   (add-subsort s1 s2 loc))]
              [(list 'op name arity rsort)
               (for/fold ([decls (~> decls
                                     (add-sort rsort loc)
                                     (add-op name arity rsort loc))])
                         ([arg arity])
                 (match arg
                   [(list 'var name sort)
                    (add-var decls name sort loc)]
                   [(list 'sort s)
                    decls]))]
              [(list 'var name sort)
               (add-var decls name sort loc)] 
              [(list 'rule pattern replacement clauses)
               (add-rule decls pattern replacement clauses loc)]
              [(list 'equation left right clauses)
               (add-equation decls left right clauses loc)])
            )
          (hash-update 'rules reverse)))

    (new-context- name decls))

  (define (new-context-from-declarations name include-refs context-decls loc)
    (new-context- name
                  (~> context-decls
                      (hash-update 'includes
                                   (λ (is) (append is (map car include-refs))))
                      (hash-set 'locs (λ (x) loc)))))

  (define (new-context- name cdecls)
    (define locs (hash-ref cdecls 'locs))
    (define included-contexts
        (for/list ([name (hash-ref cdecls 'includes)])
          (with-handlers ([exn:fail? (re-raise-exn (get-loc locs name))])
            (get-context name))))
    (define sorts (make-sort-graph included-contexts
                                   (hash-ref cdecls 'sorts)
                                   (hash-ref cdecls 'subsorts)
                                   locs))
    (define signature (make-signature sorts included-contexts
                                      (hash-ref cdecls 'ops)
                                      locs))
    (define varset (make-varset signature included-contexts
                                (hash-ref cdecls 'vars)
                                locs))
    (define rules (make-rulelist signature varset included-contexts
                                 (hash-ref cdecls 'rules)
                                 locs))
    (define equations (make-equationset signature varset included-contexts
                                        (hash-ref cdecls 'equations)
                                        locs))
    (define context (contexts:make-context sorts
                                           signature
                                           varset
                                           rules
                                           equations))
    (document (hash-set contexts name context)
              (hash-set decls name cdecls)
              library))

  (define (get-context name)
    (define elements (map string-trim (string-split name "/")))
    (case (length elements)
      [(1)
       (unless (hash-has-key? contexts (first elements))
         (error (format "no context named ~a" name)))
       (hash-ref contexts (first elements))]
      [(2)
       (unless (hash-has-key? library (first elements))
         (error (format "no library named ~a" (first elements))))
       (define library-doc (hash-ref library (first elements)))
       (send library-doc get-context (second elements))]
      [else
       (error (format "illegal context specification ~a" name))]))

  (define (get-context-declarations name)
    (define elements (map string-trim (string-split name "/")))
    (case (length elements)
      [(1)
       (unless (hash-has-key? contexts (first elements))
         (error (format "no context named ~a" name)))
       (hash-ref decls (first elements))]
      [(2)
       (unless (hash-has-key? library (first elements))
         (error (format "no library named ~a" (first elements))))
       (define library-doc (hash-ref library (first elements)))
       (send library-doc get-context-declarations (second elements))]
      [else
       (error (format "illegal context specification ~a" name))]))

  (define (get-document-sxml)
    `(*TOP* (context-collection
             ,@(for/list ([name (hash-keys contexts)])
                 (get-context-sxml name)))))

  (define (get-context-sxml name)

    (define (term->sxml term)
      (match term
        [(list 'term op args)
         `(term (@ (op ,(symbol->string op)))
                ,@(for/list ([arg args])
                    (term->sxml arg)))]
        [(list (and number-tag (or 'integer 'rational 'floating-point)) x)
         `(,number-tag (@ (value ,(format "~a" x))))]))

    (define (group-clauses clauses)
      (define (is-var? c) (equal? (first c) 'var))
      (define conditions (filter (λ (c) (not (is-var? c))) clauses))
      (define vars (filter is-var? clauses))
      (values conditions vars))

    (define (vars->sxml vars)
      (for/list ([vd vars])
        `(var (@ (id ,(symbol->string (second vd)))
                 (sort ,(symbol->string (third vd)))))))

    (define (condition->sxml conditions)
      (if (empty? conditions)
          `(condition)
          `(condition ,(term->sxml (first conditions)))))

    (define cdecls (hash-ref decls name))

    `(context (@ (id ,name))
              (includes ,@(for/list ([name (hash-ref cdecls 'includes)])
                            `(context-ref ,name)))
              (sorts ,@(for/list ([s (hash-ref cdecls 'sorts)])
                         `(sort (@ (id ,(symbol->string s))))))
              (subsorts ,@(for/list ([sd (hash-ref cdecls 'subsorts)])
                            `(subsort (@ (subsort ,(symbol->string (car sd)))
                                         (supersort ,(symbol->string (cdr sd)))))))
              (vars ,@(for/list ([vd (hash-ref cdecls 'vars)])
                        `(var (@ (id ,(symbol->string (second vd)))
                                 (sort ,(symbol->string (third vd)))))))
              (ops ,@(for/list ([od (hash-ref cdecls 'ops)])
                       `(op (@ (id ,(symbol->string (first od))))
                            (arity ,@(for/list ([sv (second od)])
                                       (if (equal? (first sv) 'sort)
                                           `(sort (@ (id ,(symbol->string (second sv)))))
                                           `(var (@ (id ,(symbol->string (second sv)))
                                                    (sort ,(symbol->string (third sv))))))))
                            (sort (@ (id ,(symbol->string (third od))))))))
              (rules ,@(for/list ([rd (hash-ref cdecls 'rules)])
                         (match-let
                             ([(list 'rule pattern replacement clauses) rd])
                           (let-values ([(conditions vars) (group-clauses clauses)])
                             `(rule (vars ,@(vars->sxml vars))
                                    (pattern ,(term->sxml pattern))
                                    ,(condition->sxml conditions)
                                    (replacement ,(term->sxml replacement)))))))
              (equations ,@(for/list ([ed (hash-ref cdecls 'equations)])
                             (match-let
                                 ([(list 'equation left right clauses) ed])
                               (let-values ([(conditions vars) (group-clauses clauses)])
                                 `(equation (vars ,@(vars->sxml vars))
                                            (left ,(term->sxml left))
                                            ,(condition->sxml conditions)
                                            (right ,(term->sxml right)))))))))

  (define (make-term context-name term-expr loc)
    (define context (get-context context-name))
    (define signature (contexts:context-signature context))
    (define vars (contexts:context-vars context))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      ((make-term* signature vars) term-expr)))

  (define (make-rule context-name rule-expr loc)
    (define context (get-context context-name))
    (define signature (contexts:context-signature context))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (make-rule* signature (contexts:context-vars context) rule-expr)))

  (define (make-equation context-name equation-expr loc)
    (define context (get-context context-name))
    (define signature (contexts:context-signature context))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (make-equation* signature (contexts:context-vars context) equation-expr)))

  (define (make-test context-name rule-expr loc)
    (define context (get-context context-name))
    (define signature (contexts:context-signature context))
    (define varset (contexts:context-vars context))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (match rule-expr
        [(list 'rule pattern replacement '())
         (let* ([mt (make-pattern* signature varset)]
                [term (mt pattern)]
                [expected (mt replacement)]
                [rterm (rewrite:reduce context term)])
           (list term expected rterm))]
        [_ (error "test may not contain rule clauses")])))

  (define (write-signature-graphs directory)

    (define (delete-directory-recursive directory)
      (when (directory-exists? directory)
        (for ([item (directory-list directory)])
          (define path (build-path directory item))
          (cond
            [(file-exists? path)
             (delete-file path)]
            [(directory-exists? path)
             (delete-directory-recursive path)]))
        (delete-directory directory)))

    (define (recursive-file-list directory)
      (flatten
       (for/list ([item (directory-list directory)])
         (define path (build-path directory item))
         (cond
           [(file-exists? path)
            path]
           [(directory-exists? path)
            (recursive-file-list path)]))))

    (delete-directory-recursive directory)
    (for ([(name context) (in-hash contexts)])
      (define path (build-path directory name))
      (tools:signature->graphviz path context))

    (define dot (find-executable-path "dot"))
    (unless dot
      (displayln "Warning: dot executable not found"))
    (for ([dot-file (recursive-file-list directory)])
      (with-output-to-file (path-replace-extension dot-file #".png")
        (thunk (system* dot "-Tpng" dot-file)))
      (delete-file dot-file))))


(define empty-document (document (hash) (hash)  (hash)))

(module+ test
  (check-equal? (~> empty-document
                    (new-context-from-source
                     "test" empty
                     (list (cons '(sort foo) #f)
                           (cons '(sort bar) #f)
                           (cons '(subsort foo bar) #f)))
                    (get-context "test"))
                test-context1)
  (check-equal? (~> empty-document
                    (new-context-from-source
                     "test" empty
                     (list (cons '(subsort foo bar) #f)))
                    (get-context "test"))
                test-context1)
  (check-equal? (~> empty-document
                    (new-context-from-source
                     "test" empty
                     (list (cons '(sort foo) #f)
                           (cons '(sort bar) #f)
                           (cons '(subsort foo bar) #f)
                           (cons '(sort bar) #f)
                           (cons '(subsort foo bar) #f)
                           (cons '(sort foo) #f)))
                    (get-context "test"))
                test-context1)

  (check-equal? (~> empty-document
                    (new-context-from-source
                     "test" empty
                     (list (cons '(subsort foo bar) #f)
                           (cons '(op a-foo () foo) #f)))
                    (make-term "test" '(term a-foo ()) #f)
                    (terms:term->string))
                "foo:a-foo")

  (check-equal? (~> empty-document
                    (new-context-from-source
                     "test" empty
                     (list (cons '(subsort foo bar) #f)
                           (cons '(sort boolean) #f)
                           (cons '(op a-foo () foo) #f)
                           (cons '(op a-foo ((sort bar)) foo) #f)
                           (cons '(op a-bar ((sort foo)) bar) #f)
                           (cons '(op true () boolean) #f)
                           (cons '(op false () boolean) #f)
                           (cons '(op _∧ ((sort boolean) (sort boolean)) boolean) #f)
                           (cons '(op a-test ((sort foo)) boolean) #f)
                           (cons '(op another-test ((sort foo)) boolean) #f)
                           (cons '(rule (term a-foo ((term X ())))
                                        (term a-foo ())
                                        ((var X foo)
                                         (term a-test ((term X ())))
                                         (term another-test ((term X ()))))) #f)
                           (cons '(equation (term a-foo ())
                                            (term a-foo ((term a-foo ())))
                                            ()) #f)))
                    (get-context "test"))
                test-context2)

  (check-equal? (~> empty-document
                    (new-context-from-source
                     "test" empty
                     (list (cons '(subsort foo bar) #f)
                           (cons '(op a-foo () foo) #f)
                           (cons '(op a-foo ((sort bar)) foo) #f)
                           (cons '(op a-bar ((var X foo)) bar) #f)
                           (cons '(rule (term a-foo ((term X ())))
                                        (term a-foo ())
                                        ((var X foo))) #f)
                           (cons '(equation (term a-foo ())
                                            (term a-foo ((term a-foo ())))
                                            ()) #f)))
                    (get-context-declarations "test"))
                (hash 'includes empty
                      'locs (hash)
                      'sorts (set 'foo 'bar)
                      'subsorts (set (cons 'foo 'bar))
                      'ops (set 
                            '(a-foo () foo)
                            '(a-foo ((sort bar)) foo)
                            '(a-bar ((var X foo)) bar))
                      'vars (set '(var X foo))
                      'rules (list
                              '(rule (term a-foo ((term X ())))
                                     (term a-foo ())
                                     ((var X foo))))
                      'equations (set
                                  '(equation (term a-foo ())
                                             (term a-foo ((term a-foo ())))
                                             ()))))

  (check-equal? (~> empty-document
                    (new-context-from-source
                     "test" empty
                     (list (cons '(subsort foo bar) #f)
                           (cons '(op a-foo () foo) #f)
                           (cons '(op a-foo ((sort bar)) foo) #f)
                           (cons '(op a-bar ((var X foo)) bar) #f)
                           (cons '(rule (term a-foo ((term X ())))
                                        (term a-foo ())
                                        ((var X foo))) #f)
                           (cons '(equation (term a-foo ())
                                            (term a-foo ((term a-foo ())))
                                            ()) #f)))
                    (get-context-declarations "test"))
                (hash 'includes empty
                      'sorts (set 'foo 'bar)
                      'subsorts (set (cons 'foo 'bar))
                      'ops (set 
                            '(a-foo () foo)
                            '(a-foo ((sort bar)) foo)
                            '(a-bar ((var X foo)) bar))
                      'vars (set '(var X foo))
                      'rules (list
                              '(rule (term a-foo ((term X ())))
                                     (term a-foo ())
                                     ((var X foo))))
                      'equations (set
                                  '(equation (term a-foo ())
                                             (term a-foo ((term a-foo ())))
                                             ()))
                      'locs (hash)))

  ;; (check-exn exn:fail?
  ;;            ; non-regular signature
  ;;            (thunk
  ;;             (~> empty-document
  ;;                 (new-context-from-source "test" empty
  ;;                              (list (cons '(subsort A B) #f)
  ;;                                    (cons '(subsort A C) #f)
  ;;                                    (cons '(op foo ((sort B)) B) #f)
  ;;                                    (cons '(op foo ((sort C)) C) #f))))))

  (check-true (~> empty-document
                  (new-context-from-source
                   "test" empty
                   (list (cons '(sort foo) #f)
                         (cons '(op a-foo () foo) #f)
                         (cons '(op a-foo ((var X foo)) foo) #f)
                         (cons '(rule (term a-foo ((term X ())))
                                      (term a-foo ())
                                      ((var X foo))) #f)))
                  (make-test "test" '(rule (term a-foo ((term a-foo ())))
                                           (term a-foo ())
                                           ())
                             #f)
                  ; check if the last two (out of three) elements are equal
                  rest
                  list->set
                  set-count
                  (equal? 1))))
