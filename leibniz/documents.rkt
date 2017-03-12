#lang racket

(provide empty-document
         add-library add-context
         new-context get-context get-context-declarations
         make-term make-rule make-equation
         make-test)

(require (prefix-in sorts: "./sorts.rkt")
         (prefix-in operators: "./operators.rkt")
         (prefix-in terms: "./terms.rkt")
         (prefix-in equations: "./equations.rkt")
         (prefix-in contexts: "./contexts.rkt")
         (prefix-in rewrite: "./rewrite.rkt")
         "./lightweight-class.rkt"
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
    (=> #:var (X foo) (a-foo X) a-foo #:if (_∧ (a-test X) (another-test X)))))

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

(define (add-if-missing element list)
  (if (member element list)
      list
      (cons element list)))

(define (make-sort-graph includes decls)
  (define after-includes
    (for/fold ([ms sorts:empty-sort-graph])
              ([c includes])
      (sorts:merge-sort-graphs ms (contexts:context-sort-graph c))))
  (define-values (sort-graph sort-decls)
    (for/fold ([sorts after-includes]
               [sort-decls empty])
              ([decl/loc decls])
      (with-handlers ([exn:fail? (re-raise-exn (cdr decl/loc))])
        (define decl (car decl/loc))
        (match decl
          [(list 'sort s)
           (values (sorts:add-sort sorts s)
                   (add-if-missing decl sort-decls))]
          [(list 'subsort s1 s2)
           (values (~> sorts
                       (sorts:add-sort s1)
                       (sorts:add-sort s2)
                       (sorts:add-subsort-relation s1 s2))
                   (~>> sort-decls
                        (add-if-missing (list 'sort s1))
                        (add-if-missing (list 'sort s2))
                        (add-if-missing decl)))]
          [(list 'op op args rsort)
           (values (sorts:add-sort sorts rsort)
                   (add-if-missing (list 'sort rsort) sort-decls))]
          [_ (values sorts sort-decls)]))))
  (values sort-graph (reverse sort-decls)))


(define (make-signature sort-graph includes decls)
  (define (argsort sort-or-var-decl)
    (match sort-or-var-decl
      [(list 'sort sort-id) sort-id]
      [(list 'var var-name sort-id) sort-id]))
  (define after-includes
    (for/fold ([msig (operators:empty-signature sort-graph)])
              ([c includes])
      (operators:merge-signatures msig (contexts:context-signature c) sort-graph)))
  (define-values (signature op-decls)
    (for/fold ([sig after-includes]
               [op-decls empty])
              ([decl/loc decls])
      (define decl (car decl/loc))
      (define loc (cdr decl/loc))
      (with-handlers ([exn:fail? (re-raise-exn loc)])
        (match decl
          [(list 'op op args rsort)
           (values (operators:add-op sig op (map argsort args) rsort #:meta loc)
                   (add-if-missing decl op-decls))]
          [_ (values sig op-decls)]))))
  (define non-regular (operators:non-regular-op-example signature))
  (when non-regular
    (match-let ([(list op argsorts rsorts) non-regular])
      (define loc
        (for/first ([decl/loc decls]
                    #:when (and (equal? (first (car decl/loc)) 'op)
                                (equal? op (second (car decl/loc)))))
          (cdr decl/loc)))
      (with-handlers ([exn:fail? (re-raise-exn loc)])
        (error (format "operator ~a~a has ambiguous sorts ~a" op argsorts rsorts)))))
  (values signature (reverse op-decls)))

(define (make-term* signature)
  (letrec ([fn (match-lambda
                 [(list 'term op args)
                  (terms:make-term signature op (map fn args))]
                 [(list 'integer n) n]
                 [(list 'rational r) r]
                 [(list 'floating-point fp) fp])])
    fn))

(define (make-varset* signature clauses)
  (define sorts (operators:signature-sort-graph signature))
  (for/fold ([vs (terms:empty-varset sorts)])
            ([c clauses])
    (match c
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
                 [(list 'rational r) r])])
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

(define (make-rule* signature rule-expr)
  (match rule-expr
    [(list 'rule pattern replacement clauses)
     (let* ([varset (make-varset* signature clauses)]
            [mp (make-pattern* signature varset)])
       (equations:make-rule signature (mp pattern)
                            (mp (make-condition* clauses))
                            (mp replacement)
                            #f #t))]
    [_ #f]))

(define (make-rulelist signature includes decls)
  (define after-includes
    (for/fold ([mrl equations:empty-rulelist])
              ([c includes])
      (equations:merge-rulelists mrl (contexts:context-rules c) signature)))
  (define-values (rules rule-decls)
    (for/fold ([rl after-includes]
               [rule-decls empty])
              ([decl/loc decls])
      (with-handlers ([exn:fail? (re-raise-exn (cdr decl/loc))])
        (define decl (car decl/loc))
        (define rule (make-rule* signature decl))
        (if rule
            (values (equations:add-rule rl rule)
                    (add-if-missing decl rule-decls))
            (values rl rule-decls)))))
  (values rules (reverse rule-decls)))

(define (make-equation* signature equation-expr)
  (match equation-expr
    [(list 'equation left right clauses)
     (let* ([varset (make-varset* signature clauses)]
            [mp (make-pattern* signature varset)])
       (equations:make-equation signature
                                (mp left)
                                (mp (make-condition* clauses))
                                (mp right)
                                #f))]
    [_ #f]))

; Generate a list of declarations that extends base-context to full-context

(define (sort-graph-diff base-sort-graph full-sort-graph)
  (define base-sorts (sorts:all-sorts base-sort-graph))
  (define base-subsorts (sorts:all-subsort-relations base-sort-graph))
  (append*
   (for/list ([cc (sorts:connected-components full-sort-graph)])
     (append
      (for/list ([s (in-set (set-subtract (sorts:all-sorts cc) base-sorts))])
        (list 'sort s))
      (for/list ([ss (in-set (set-subtract (sorts:all-subsort-relations cc) base-subsorts))])
        (list 'subsort (car ss) (cdr ss)))))))

(define (op-diff base-signature full-signature)
  (define base-ops
    (for/set ([(symbol rank meta) (operators:all-ops base-signature)])
      (cons symbol rank)))
  (for/list ([(symbol rank meta) (operators:all-ops full-signature)]
             #:unless (set-member? base-ops (cons symbol rank)))
    (list 'op symbol (map (λ (s) `(sort ,s)) (car rank)) (cdr rank))))

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

(define (rulelist-diff base-rulelist full-rulelist)
  (define base-rules (for/set ([rule (equations:in-rules base-rulelist)])
                       (rule->decl rule)))
  (define full-rules (for/list ([rule (equations:in-rules full-rulelist)])
                       (rule->decl rule)))
  (filter (λ (r) (not (set-member? base-rules r))) full-rules))

(define (context-diff base-context full-context)
  (define base-signature (contexts:context-signature base-context))
  (define full-signature (contexts:context-signature full-context))
  (define base-sort-graph (operators:signature-sort-graph base-signature))
  (define full-sort-graph (operators:signature-sort-graph full-signature))
  (hash 'sorts (sort-graph-diff base-sort-graph full-sort-graph)
        'ops (op-diff base-signature full-signature)
        'rules (rulelist-diff (contexts:context-rules base-context)
                              (contexts:context-rules full-context))))

; Documents track declarations and expressions embedded in a Scribble document

(define-class document

  (field contexts includes decls library)
  ; contexts: a hash mapping context names (strings) to contexts
  ; includes: a hash mapping context names (strings) to lists of includes (strings)
  ; decls: a hash with keys 'sorts 'ops 'rules, values are lists of the declarations
  ;        in each category
  ; library: a hash mapping document names to documents

  (define (add-library name library-document)
    (document contexts
              includes
              decls
              (hash-set library name library-document)))

  (define (add-context name include-refs context)
    (define included-contexts
      (for/list ([name/loc include-refs])
        (with-handlers ([exn:fail? (re-raise-exn (cdr name/loc))])
          (get-context (car name/loc)))))
    (define-values (sorts sort-decls)
      (make-sort-graph included-contexts empty))
    (define-values (signature op-decls)
      (make-signature sorts included-contexts empty))
    (define-values (rules rule-decls)
      (make-rulelist signature included-contexts empty))
    (define inclusion-context
      (contexts:builtin-context sorts
                                signature
                                (terms:empty-varset sorts)
                                rules
                                equations:empty-equationset))
    (define new-context
      (contexts:merge-contexts inclusion-context context))
    (define added-decls (context-diff inclusion-context new-context))
    (document (hash-set contexts name new-context)
              (hash-set includes name (map car include-refs))
              (hash-set decls name added-decls)
              library))

  (define (new-context name include-refs context-decls)
    (define included-contexts
      (for/list ([name/loc include-refs])
        (with-handlers ([exn:fail? (re-raise-exn (cdr name/loc))])
          (get-context (car name/loc)))))
    (define-values (sorts sort-decls)
      (make-sort-graph included-contexts context-decls))
    (define-values (signature op-decls)
      (make-signature sorts included-contexts context-decls))
    (define-values (rules rule-decls)
      (make-rulelist signature included-contexts context-decls))
    (define context
      (contexts:builtin-context sorts
                                signature
                                (terms:empty-varset sorts)
                                rules
                                equations:empty-equationset))
    (document (hash-set contexts name context)
              (hash-set includes name (map car include-refs))
              (hash-set decls name (hash 'sorts sort-decls 'ops op-decls 'rules rule-decls))
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

  (define (make-term context-name term-expr loc)
    (define context (get-context context-name))
    (define signature (contexts:context-signature context))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      ((make-term* signature) term-expr)))

  (define (make-rule context-name rule-expr loc)
    (define context (get-context context-name))
    (define signature (contexts:context-signature context))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (make-rule* signature rule-expr)))

  (define (make-equation context-name equation-expr loc)
    (define context (get-context context-name))
    (define signature (contexts:context-signature context))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (make-equation* signature equation-expr)))

  (define (make-test context-name rule-expr loc)
    (define context (get-context context-name))
    (define signature (contexts:context-signature context))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (match rule-expr
        [(list 'rule pattern replacement '())
         (let* ([mt (make-term* signature)]
                [term (mt pattern)]
                [expected (mt replacement)]
                [rterm (rewrite:reduce context term)])
           (list term expected rterm))]
        [_ (error "test may not contain rule clauses")]))))


(define empty-document (document (hash) (hash) (hash)  (hash)))

(module+ test
  (check-equal? (~> empty-document
                    (new-context "test" empty
                                 (list (cons '(sort foo) #f)
                                       (cons '(sort bar) #f)
                                       (cons '(subsort foo bar) #f)))
                    (get-context "test"))
                test-context1)
  (check-equal? (~> empty-document
                    (new-context "test" empty
                                 (list (cons '(subsort foo bar) #f)))
                    (get-context "test"))
                test-context1)
  (check-equal? (~> empty-document
                    (new-context "test" empty
                                 (list (cons '(sort foo) #f)
                                       (cons '(sort bar) #f)
                                       (cons '(subsort foo bar) #f)
                                       (cons '(sort bar) #f)
                                       (cons '(subsort foo bar) #f)
                                       (cons '(sort foo) #f)))
                    (get-context "test"))
                test-context1)

  (check-equal? (~> empty-document
                    (new-context "test" empty
                                 (list (cons '(subsort foo bar) #f)
                                       (cons '(op a-foo () foo) #f)))
                    (make-term "test" '(term a-foo ()) #f)
                    (terms:term->string))
                "foo:a-foo")

  (check-equal? (~> empty-document
                    (new-context "test" empty
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
                                       ))
                    (get-context "test"))
                test-context2)

  (check-equal? (~> empty-document
                    (new-context "test" empty
                                 (list (cons '(subsort foo bar) #f)
                                       (cons '(op a-foo () foo) #f)
                                       (cons '(op a-foo ((sort bar)) foo) #f)
                                       (cons '(op a-bar ((var X foo)) bar) #f)
                                       (cons '(rule (term a-foo ((term X ())))
                                                    (term a-foo ())
                                                    ((var X foo))) #f)))
                    (get-context-declarations "test"))
                (hash 'sorts (list '(sort foo)
                                   '(sort bar)
                                   '(subsort foo bar))
                      'ops (list 
                            '(op a-foo () foo)
                            '(op a-foo ((sort bar)) foo)
                            '(op a-bar ((var X foo)) bar))
                      'rules (list
                              '(rule (term a-foo ((term X ())))
                                     (term a-foo ())
                                     ((var X foo))))))

  (check-exn exn:fail?
             ; non-regular signature
             (thunk
              (~> empty-document
                  (new-context "test" empty
                               (list (cons '(subsort A B) #f)
                                     (cons '(subsort A C) #f)
                                     (cons '(op foo ((sort B)) B) #f)
                                     (cons '(op foo ((sort C)) C) #f))))))

  (check-true (~> empty-document
                  (new-context "test" empty
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
