#lang racket

(provide
 (contract-out
  [reduce (context? term? . -> . term?)]
  [reduce-equation ((context? equation?) ((or/c #f symbol?))
                    . ->* . equation?)]
  [transform (context? transformation? term? . -> . term?)]
  [transform-equation ((context? transformation? equation?) ((or/c #f symbol?))
                       . ->* . equation?)]))

(require "./sorts.rkt"
         "./operators.rkt"
         "./terms.rkt"
         "./builtins.rkt"
         "./equations.rkt"
         "./contexts.rkt"
         racket/generator)

(module+ test

  (require "./term-syntax.rkt"
           "./test-examples.rkt"
           rackunit
           racket/function
           rackjure/threading)

  (define-context test-context
    (sort Boolean)
    (op true Boolean)
    (op false Boolean)
    (op (not Boolean) Boolean)
    (op foo Boolean)
    (=> (not true) false)
    (=> (not false) true)
    (=> foo (not true) #:if false)
    (=> foo (not false) #:if true)
    (eq #:label an-equation
        foo true))

  (define-context test-with-var
    (sort Boolean)
    (sort FooBar)
    (op true Boolean)
    (op false Boolean)
    (op (not Boolean) Boolean)
    (op foo Boolean)
    (op bar Boolean)
    (op foo-bar FooBar)
    (var X Boolean)
    (=> (not true) false)
    (=> (not false) true)
    (=> (not (not X)) X))

  (define-context test-with-procedure
    (sort Boolean)
    (op true Boolean)
    (op false Boolean)
    (op (not Boolean) Boolean)
    (var X Boolean)
    (-> (not X) (λ (signature pattern condition substitution)
                  (let ([x (substitution-value substitution 'X)])
                    (define-values (op args) (term.op-and-args x))
                    (cond
                      [(equal? op 'true)
                       (make-term signature 'false empty)]
                      [(equal? op 'false)
                       (make-term signature 'true empty)]
                      [(equal? op 'not)
                       (first args)]))))))

;
; Rule matching and basic term rewriting
;
(define (test-condition context condition substitution)
  (define signature (context-signature context))
  (or (not condition)
      (let* ([s-condition (term.substitute signature condition substitution)]
             [r-condition (reduce context s-condition)])
        (let-values ([(op args) (term.op-and-args r-condition)])
          (and (equal? op 'true)
               (empty? args))))))

(define (in-matching-rules context term test-conditions?)
  (define signature (context-signature context))
  (define rules (lookup-rules (context-rules context) term))
  (unless (allowed-term? signature term)
    (error (format "term ~s not allowed by context\n~s" term context)))
  (in-generator #:arity 2
   (for* ([rule rules]
          [s (term.match signature (rule-pattern rule) term)])
     (when (or (not test-conditions?)
               (test-condition context (rule-condition rule) s))
       (yield rule s)))))

(define (all-matching-rules context term test-conditions?)
  (for/list ([(rule substitution)
              (in-matching-rules context term test-conditions?)])
    (cons rule substitution)))

(module+ test
  (with-context test-context
    (define signature (context-signature test-context))
    (check-equal? (all-matching-rules test-context (T (not true)) #f)
                  (list (cons (make-rule signature (T (not true)) #f (T false) #f #t)
                              empty-substitution)))
    (check-equal? (all-matching-rules test-context (T foo) #f)
                  (list (cons (make-rule signature
                                         (T foo) (T false) (T (not true)) #f #t)
                              empty-substitution)
                        (cons (make-rule signature
                                         (T foo) (T true) (T (not false)) #f #t)
                              empty-substitution)))
    (check-equal? (all-matching-rules test-context (T foo) #t)
                  (list (cons (make-rule signature
                                         (T foo) (T true) (T (not false)) #f #t)
                              empty-substitution)))))

(define (substitute signature rule substitution)
  (define replacement (rule-replacement rule))
  (if (procedure? replacement)
      (with-handlers ([exn:fail? (lambda (v) #f)])
        (replacement signature
                     (rule-pattern rule)
                     (rule-condition rule)
                     substitution))
      (term.substitute signature replacement substitution)))

(define (rewrite-head-once context term)
  (define signature (context-signature context))
  (or (for/first ([(rule substitution) (in-matching-rules context term #t)])
        (substitute signature rule substitution))
      term))

(module+ test

  (with-context test-context
    (check-equal? (rewrite-head-once test-context (T (not true)))
                  (T false))
    (check-equal? (rewrite-head-once test-context (T (not false)))
                  (T true))
    (check-equal? (rewrite-head-once test-context (T (not (not false))))
                  (T (not (not false))))
    (check-equal? (rewrite-head-once test-context (T foo))
                  (T (not false)))
    (check-exn exn:fail? (thunk (rewrite-head-once test-context (T 42)))))

  (with-context test-with-var
    (check-equal? (rewrite-head-once test-with-var (T (not true)))
                  (T false))
    (check-equal? (rewrite-head-once test-with-var (T (not false)))
                  (T true))
    (check-equal? (rewrite-head-once test-with-var (T (not (not false))))
                  (T false))
    (check-exn exn:fail? (thunk (rewrite-head-once test-with-var (T 42)))))

  (with-context test-with-procedure
    (check-equal? (rewrite-head-once test-with-procedure (T (not true)))
                  (T false))
    (check-equal? (rewrite-head-once test-with-procedure (T (not false)))
                  (T true))
    (check-equal? (rewrite-head-once test-with-procedure (T (not (not false))))
                  (T false))
    (check-exn exn:fail?
               (thunk (rewrite-head-once test-with-procedure (T 42))))))

;
; Recursive rewriting (one step)
;
(define (rewrite-leftmost-innermost context term)
  (define-values (op args) (term.op-and-args term))
  (define signature (context-signature context))
  (if op
      ; op-term or op-pattern: first reduce args
      (let* ([reduced-args (map (λ (arg) (reduce context arg)) args)]
             [with-reduced-args (if (andmap eq? args reduced-args)
                                    ; Optimization for unchanged args, avoids
                                    ; unnecessary term construction. Note the
                                    ; use of eq? for doing a fast check.
                                    term
                                    (make-term* signature op reduced-args))])
        (rewrite-head-once context with-reduced-args))
      ; builtin term type: no args
      (rewrite-head-once context term)))

(module+ test
  (with-context test-context
    (check-equal? (rewrite-leftmost-innermost test-context (T (not true)))
                  (T false))
    (check-equal? (rewrite-leftmost-innermost test-context (T (not false)))
                  (T true))
    (check-equal? (rewrite-leftmost-innermost test-context
                                              (T (not (not false))))
                  (T false))
    (check-equal? (rewrite-leftmost-innermost test-context (T foo))
                  (T (not false)))))

;
; Recursive rewriting to normal form
;
(define (reduce context term)
  (let loop ([term term])
    (let* ([rewritten-term (rewrite-leftmost-innermost context term)])
      (cond
        ; First a fast eq? check
        [(eq? rewritten-term term)    term]
        ; Next the more general but slow equal? check
        [(equal? rewritten-term term) term]
        ; Not equal: another round of rewriting
        [else                         (loop rewritten-term)]))))

(module+ test
  (with-context test-context
    (check-equal? (reduce test-context (T (not true)))
                  (T false))
    (check-equal? (reduce test-context (T (not false)))
                  (T true))
    (check-equal? (reduce test-context (T (not (not false))))
                  (T false))
    (check-equal? (reduce test-context (T foo))
                  (T true))))

;
; Reduction of equations: reduce left and right
;
(define (reduce-equation context equation [new-label #f])
  (make-equation (context-signature context)
                 (reduce context (equation-left equation))
                 (equation-condition equation)
                 (reduce context (equation-right equation))
                 new-label))

(module+ test
  (with-context test-context
    (check-equal? (reduce-equation test-context (eq an-equation))
                  (eq true true))
    (check-equal? (reduce-equation test-context (eq an-equation) 'new-equation)
                  (eq #:label new-equation true true))))

;
; Transformations of terms and equations
;
(define (transform context transformation term)
  (define signature (context-signature context))
  (define rule (transformation-converted-rule transformation))
  ; Check if the transformation introduced new vars that are in
  ; conflict with existing ones.
  (define new-vars (set-subtract (term.vars (rule-replacement rule))
                                 (term.vars (rule-pattern rule))))
  (define term-vars (term.vars term))
  (unless (or (set-empty? new-vars)
              (set-empty? term-vars))
    (define (var-hash vars)
      (make-hash (for/list ([v vars])
                   (cons (var-name v) (var-sort v)))))
    (define tvars-as-hash (var-hash term-vars))
    (for/first ([v new-vars]
                #:when (not (equal? (var-sort v)
                                    (hash-ref tvars-as-hash (var-name v)
                                              (var-sort v)))))
      (error (format "Var ~s has sort ~s in transformation but sort ~s in the term to be transformed" (var-name v) (var-sort v) (hash-ref tvars-as-hash (var-name v))))))
  ; Apply the transformation
  (reduce context
          (substitute signature rule
                      (one-match signature (rule-pattern rule) term))))

(define (transform-equation context transformation equation [new-label #f])
  (make-equation (context-signature context)
                 (transform context transformation (equation-left equation))
                 (equation-condition equation)
                 (transform context transformation (equation-right equation))
                 new-label))

(module+ test
  (with-context test-with-var
    (define transformation (tr #:var (X Boolean) X (not X)))
    (check-equal? (transform test-with-var transformation (T foo))
                  (T (not foo)))
    (check-equal? (transform test-with-var transformation (T X))
                  (T (not X)))
    (check-equal? (transform test-with-var transformation (T (not X)))
                  (T X))
    (check-equal? (transform-equation test-with-var transformation
                                      (eq foo bar))
                  (eq (not foo) (not bar)))
    (check-equal? (transform-equation test-with-var transformation
                                      (eq #:label original foo bar)
                                      'negated)
                  (eq #:label negated (not foo) (not bar))))
  (with-context test-with-var
    (define transformation (tr #:vars ([% Boolean] [Y FooBar]) % Y))
    (check-exn exn:fail?
               (thunk (transform test-with-var transformation
                                 (T #:var (Y Boolean) (not Y))))))
  (with-context test-context
    (define transformation (tr #:var (X Boolean) X (not X)))
    (check-equal? (transform-equation test-context transformation
                                      (eq an-equation))
                  (eq false false))))
