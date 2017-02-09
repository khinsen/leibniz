#lang racket

(provide empty-document
         add-declaration)

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

; Lazy signatures store declarations to be added until an explicit version
; is requested.

(struct lazy-signature (base-signature declarations))

(define (lazy-add-declaration l-signature declaration)
  (lazy-signature (lazy-signature-base-signature l-signature)
                  (cons declaration (lazy-signature-declarations l-signature))))

(define (lazy-make-explicit l-signature)
  (define base (lazy-signature-base-signature l-signature))
  (define base-sorts (operators:signature-sort-graph base))
  (define declarations (reverse (lazy-signature-declarations l-signature)))
  (define sorts
    (for/fold ([sorts base-sorts])
              ([decl declarations])
      (match decl
        [(list 'sort s) (sorts:add-sort sorts s)]
        [(list 'subsort s1 s2) (~> sorts
                                   (sorts:add-sort s1)
                                   (sorts:add-sort s2)
                                   (sorts:add-subsort-relation s1 s2))]
        [_ sorts])))
  ; TODO operators
  (operators:empty-signature sorts))

(module+ test
  (define empty-lazy-signature
    (lazy-signature (operators:empty-signature sorts:empty-sort-graph) empty))
  (check-equal? (~> empty-lazy-signature
                    (lazy-add-declaration '(sort foo))
                    (lazy-add-declaration '(sort bar))
                    (lazy-add-declaration '(subsort foo bar))
                    (lazy-make-explicit))
                (contexts:context-signature test-context))
  (check-equal? (~> empty-lazy-signature
                    (lazy-add-declaration '(subsort foo bar))
                    (lazy-make-explicit))
                (contexts:context-signature test-context))
  (check-equal? (~> empty-lazy-signature
                    (lazy-add-declaration '(sort foo))
                    (lazy-add-declaration '(sort bar))
                    (lazy-add-declaration '(sort foo))
                    (lazy-add-declaration '(subsort foo bar))
                    (lazy-add-declaration '(sort bar))
                    (lazy-add-declaration '(subsort foo bar))
                    (lazy-make-explicit))
                (contexts:context-signature test-context)))

; Documents track declarations and expressions embedded in a Scribble document

(define-class document

  (field signature context named-contexts)
  ; signature: a lazy signature including all declarations seen so far
  ; named-contexts: a hash mapping context names (strings) to contexts

  (define (add-declaration decl)
    (document (lazy-add-declaration signature decl)
              context
              named-contexts))

  (define (process-declarations)
    (define sig (lazy-make-explicit signature))
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
                    (add-declaration '(subsort foo bar))
                    (process-declarations)
                    (current-context)
                    )
                test-context)
)
