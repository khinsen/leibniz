#lang racket

(provide empty-document
         add-library add-builtin-context
         add-context get-context
         make-term make-rule make-equation)

(require (prefix-in sorts: "./sorts.rkt")
         (prefix-in operators: "./operators.rkt")
         (prefix-in terms: "./terms.rkt")
         (prefix-in equations: "./equations.rkt")
         (prefix-in contexts: "./contexts.rkt")
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
    (sort Boolean)
    (subsort foo bar)
    (op a-foo foo)
    (op (a-foo bar) foo)
    (op (a-bar foo) bar)
    (op true Boolean)
    (op false Boolean)
    (op (_∧ Boolean Boolean) Boolean)
    (op (a-test foo) Boolean)
    (op (another-test foo) Boolean)
    (=> #:var (X foo) (a-foo X) a-foo #:if (_∧ (a-test X) (another-test X)))
    ))

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

(define (make-sort-graph includes decls)
  (define after-includes
    (for/fold ([ms sorts:empty-sort-graph])
              ([c includes])
      (sorts:merge-sort-graphs ms (contexts:context-sort-graph c))))
  (for/fold ([sorts after-includes])
            ([decl/loc decls])
    (with-handlers ([exn:fail? (re-raise-exn (cdr decl/loc))])
      (match (car decl/loc)
        [(list 'sort s) (sorts:add-sort sorts s)]
        [(list 'subsort s1 s2) (~> sorts
                                   (sorts:add-sort s1)
                                   (sorts:add-sort s2)
                                   (sorts:add-subsort-relation s1 s2))]
        [(list (or 'prefix-op 'infix-op 'special-op) op args rsort) (sorts:add-sort sorts rsort)]
        [_ sorts]))))

(define (make-signature sort-graph includes decls)
  (define after-includes
    (for/fold ([msig (operators:empty-signature sort-graph)])
              ([c includes])
      (operators:merge-signatures msig (contexts:context-signature c) sort-graph)))
  (define signature
    (for/fold ([sig after-includes])
              ([decl/loc decls])
      (define loc (cdr decl/loc))
      (with-handlers ([exn:fail? (re-raise-exn loc)])
        (match (car decl/loc)
          [(list 'prefix-op op args rsort)
           (operators:add-op sig op args rsort #:meta loc)]
          [(list 'infix-op op args rsort)
           (operators:add-op sig op args rsort #:meta loc)]
          [(list 'special-op op args rsort)
           (operators:add-op sig op args rsort #:meta loc)]
          [_ sig]))))
  (define non-regular (operators:non-regular-op-example signature))
  (when non-regular
    (match-let ([(list op argsorts rsorts) non-regular])
      (define loc
        (for/first ([decl/loc decls]
                    #:when (and (member (first (car decl/loc)) '(prefix-op infix-op special-op))
                                (equal? op (second (car decl/loc)))))
          (cdr decl/loc)))
      (with-handlers ([exn:fail? (re-raise-exn loc)])
        (error (format "operator ~a~a has ambiguous sorts ~a" op argsorts rsorts)))))
  signature)

(define (make-term* signature)
  (letrec ([fn (match-lambda
                 [(list 'term op args)
                  (terms:make-term signature op (map fn args))]
                 [(list 'integer n) n]
                 [(list 'rational r) r])])
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
  (for/fold ([rl after-includes])
            ([decl/loc decls])
    (with-handlers ([exn:fail? (re-raise-exn (cdr decl/loc))])
      (define rule (make-rule* signature (car decl/loc)))
      (if rule
          (equations:add-rule rl rule)
          rl))))

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

; Documents track declarations and expressions embedded in a Scribble document

(define-class document

  (field contexts library)
  ; contexts: a hash mapping context names (strings) to contexts
  ; library: a hash mapping document names to documents

  (define (add-library name library-document)
    (document contexts (hash-set library name library-document)))

  (define (add-builtin-context name context)
    (document (hash-set contexts name context) library))

  (define (add-context name includes decls)
    (define included-contexts
      (for/list ([name/loc includes])
        (with-handlers ([exn:fail? (re-raise-exn (cdr name/loc))])
          (get-context (car name/loc)))))
    (define sorts (make-sort-graph included-contexts decls))
    (define signature (make-signature sorts included-contexts decls))
    (define rules (make-rulelist signature included-contexts decls))
    (define context
      (contexts:builtin-context sorts
                                signature
                                (terms:empty-varset sorts)
                                rules
                                equations:empty-equationset))
    (document (hash-set contexts name context) library))

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
      (make-equation* signature equation-expr))))


(define empty-document (document (hash) (hash)))

(module+ test
  (check-equal? (~> empty-document
                    (add-context "test" empty
                                 (list (cons '(sort foo) #f)
                                       (cons '(sort bar) #f)
                                       (cons '(subsort foo bar) #f)))
                    (get-context "test"))
                test-context1)
  (check-equal? (~> empty-document
                    (add-context "test" empty
                                 (list (cons '(subsort foo bar) #f)))
                    (get-context "test"))
                test-context1)
  (check-equal? (~> empty-document
                    (add-context "test" empty
                                 (list (cons '(sort foo) #f)
                                       (cons '(sort bar) #f)
                                       (cons '(subsort foo bar) #f)
                                       (cons '(sort bar) #f)
                                       (cons '(subsort foo bar) #f)
                                       (cons '(sort foo) #f)))
                    (get-context "test"))
                test-context1)

  (check-equal? (~> empty-document
                    (add-context "test" empty
                                 (list (cons '(subsort foo bar) #f)
                                       (cons '(prefix-op a-foo () foo) #f)))
                    (make-term "test" '(term a-foo ()) #f)
                    (terms:term->string))
                "foo:a-foo")

  (check-equal? (~> empty-document
                    (add-context "test" empty
                                 (list (cons '(subsort foo bar) #f)
                                       (cons '(sort Boolean) #f)
                                       (cons '(prefix-op a-foo () foo) #f)
                                       (cons '(prefix-op a-foo (bar) foo) #f)
                                       (cons '(prefix-op a-bar (foo) bar) #f)
                                       (cons '(prefix-op true () Boolean) #f)
                                       (cons '(prefix-op false () Boolean) #f)
                                       (cons '(prefix-op _∧ (Boolean Boolean) Boolean) #f)
                                       (cons '(prefix-op a-test (foo) Boolean) #f)
                                       (cons '(prefix-op another-test (foo) Boolean) #f)
                                       (cons '(rule (term a-foo ((term X ())))
                                                    (term a-foo ())
                                                    ((var X foo)
                                                     (term a-test ((term X ())))
                                                     (term another-test ((term X ()))))) #f)
                                       ))
                    (get-context "test"))
                test-context2))
