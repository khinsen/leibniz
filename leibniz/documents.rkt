#lang racket

(provide empty-document
         add-declaration
         process-declarations)

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

; Lazy signatures store declarations to be added until an explicit version
; is requested.

(struct lazy-signature (base-signature declarations))

(define (lazy-add-declaration l-signature declaration loc)
  (lazy-signature (lazy-signature-base-signature l-signature)
                  (cons (cons declaration loc)
                        (lazy-signature-declarations l-signature))))

(define (lazy-make-explicit l-signature)
  (define base (lazy-signature-base-signature l-signature))
  (define base-sorts (operators:signature-sort-graph base))
  (define declarations (reverse (lazy-signature-declarations l-signature)))
  (define sorts
    (for/fold ([sorts base-sorts])
              ([decl/loc declarations])
      (with-handlers ([exn:fail? (re-raise-exn (cdr decl/loc))])
        (match (car decl/loc)
          [(list 'sort s) (sorts:add-sort sorts s)]
          [(list 'subsort s1 s2) (~> sorts
                                     (sorts:add-sort s1)
                                     (sorts:add-sort s2)
                                     (sorts:add-subsort-relation s1 s2))]
          [_ sorts]))))
  (define signature
    (for/fold ([sig (operators:merge-signatures base (operators:empty-signature sorts) sorts)])
              ([decl/loc declarations])
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
        (for/first ([decl/loc declarations]
                    #:when (and (member (first (car decl/loc)) '(prefix-op infix-op special-op))
                                (equal? op (second (car decl/loc)))))
          (cdr decl/loc)))
      (with-handlers ([exn:fail? (re-raise-exn loc)])
        (error (format "operator ~a~a has ambiguous sorts ~a" op argsorts rsorts)))))
  signature)

(module+ test
  (define empty-lazy-signature
    (lazy-signature (operators:empty-signature sorts:empty-sort-graph) empty))
  (check-equal? (~> empty-lazy-signature
                    (lazy-add-declaration '(sort foo) #f)
                    (lazy-add-declaration '(sort bar) #f)
                    (lazy-add-declaration '(subsort foo bar) #f)
                    (lazy-make-explicit))
                (contexts:context-signature test-context))
  (check-equal? (~> empty-lazy-signature
                    (lazy-add-declaration '(subsort foo bar) #f)
                    (lazy-make-explicit))
                (contexts:context-signature test-context))
  (check-equal? (~> empty-lazy-signature
                    (lazy-add-declaration '(sort foo) #f)
                    (lazy-add-declaration '(sort bar) #f)
                    (lazy-add-declaration '(sort foo) #f)
                    (lazy-add-declaration '(subsort foo bar) #f)
                    (lazy-add-declaration '(sort bar) #f)
                    (lazy-add-declaration '(subsort foo bar) #f)
                    (lazy-make-explicit))
                (contexts:context-signature test-context)))

; Documents track declarations and expressions embedded in a Scribble document

(define-class document

  (field signature context named-contexts)
  ; signature: a lazy signature including all declarations seen so far
  ; named-contexts: a hash mapping context names (strings) to contexts

  (define (add-declaration decl loc)
    (document (lazy-add-declaration signature decl loc)
              context
              named-contexts))

  (define (process-declarations)
    (define sig  (lazy-make-explicit signature))
    (define sorts (operators:signature-sort-graph sig))
    (define context (contexts:builtin-context sorts
                                              sig
                                              (terms:empty-varset sorts)
                                              equations:empty-rulelist
                                              equations:empty-equationset))
    (document (lazy-signature sig empty) context named-contexts))

  (define (current-context*)
    context)

  (define (current-context)
    (send (process-declarations) current-context*))
)

(define empty-document
  (let* ([signature (operators:empty-signature sorts:empty-sort-graph)]
         [sorts (operators:signature-sort-graph signature)]
         [context (contexts:builtin-context sorts
                                            signature
                                            (terms:empty-varset sorts)
                                            equations:empty-rulelist
                                            equations:empty-equationset)])
    (document (lazy-signature signature empty) context (hash ))))

(module+ test

  (check-equal? (~> empty-document
                    (add-declaration '(subsort foo bar) #f)
                    (process-declarations)
                    (current-context)
                    )
                test-context)
)
