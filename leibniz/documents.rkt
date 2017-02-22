#lang racket

(provide empty-document
         add-context
         get-context)

(require (prefix-in sorts: "./sorts.rkt")
         (prefix-in operators: "./operators.rkt")
         (prefix-in terms: "./terms.rkt")
         (prefix-in equations: "./equations.rkt")
         (prefix-in contexts: "./contexts.rkt")
         "./lightweight-class.rkt"
         threading)

(module+ test
  (require rackunit)

  (contexts:define-context test-context
    (sort foo)
    (sort bar)
    (subsort foo bar)))

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
      (with-handlers ([exn:fail? (re-raise-exn (cdr decl/loc))])
        (match (car decl/loc)
          [(list 'prefix-op op args rsort) (operators:add-op sig op args rsort)]
          [(list 'infix-op op args rsort) (operators:add-op sig op args rsort)]
          [(list 'special-op op args rsort) (operators:add-op sig op args rsort)]
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

; Documents track declarations and expressions embedded in a Scribble document

(define-class document

  (field contexts)
  ; contexts: a hash mapping context names (strings) to contexts

  (define (add-context name includes decls)
    (define included-contexts
      (for/list ([name/loc includes])
        (with-handlers ([exn:fail? (re-raise-exn (cdr name/loc))])
          (get-context (car name/loc)))))
    (define sorts (make-sort-graph included-contexts decls))
    (define signature (make-signature sorts included-contexts decls))
    (define context
      (contexts:builtin-context sorts
                                signature
                                (terms:empty-varset sorts)
                                equations:empty-rulelist
                                equations:empty-equationset))
    (document (hash-set contexts name context)))

  (define (get-context name)
    (unless (hash-has-key? contexts name)
      (error (format "no context named ~a" name)))
    (hash-ref contexts name)))


(define empty-document (document (hash)))

(module+ test
  (check-equal? (~> empty-document
                    (add-context "test" (list (cons '(sort foo) #f)
                                              (cons '(sort bar) #f)
                                              (cons '(subsort foo bar) #f)))
                    (get-context "test"))
                test-context)
  (check-equal? (~> empty-document
                    (add-context "test" (list (cons '(subsort foo bar) #f)))
                    (get-context "test"))
                test-context)
  (check-equal? (~> empty-document
                    (add-context "test" (list (cons '(sort foo) #f)
                                              (cons '(sort bar) #f)
                                              (cons '(subsort foo bar) #f)
                                              (cons '(sort bar) #f)
                                              (cons '(subsort foo bar) #f)
                                              (cons '(sort foo) #f)))
                    (get-context "test"))
                test-context))
