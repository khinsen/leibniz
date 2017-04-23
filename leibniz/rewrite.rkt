#lang racket

(provide
 (contract-out
  [reduce (signature? rulelist? term? . -> . term?)]
  [trace-reduce (signature? rulelist? term? . -> . term?)]
  [reduce-equation ((signature? rulelist? equation?) ((or/c #f symbol?))
                    . ->* . equation?)]
  [transform (signature? rulelist? transformation? term? . -> . term?)]
  [transform-equation ((signature? rulelist? transformation? equation?) ((or/c #f symbol?))
                       . ->* . equation?)]
  [substitute (signature? rulelist? transformation? term? . -> . term?)]
  [substitute-equation ((signature? rulelist? transformation? equation?) ((or/c #f symbol?))
                        . ->* . equation?)]))

(require "./sorts.rkt"
         "./operators.rkt"
         "./terms.rkt"
         "./builtins.rkt"
         "./equations.rkt"
         racket/generator
         racket/trace)

(module+ test
  (require "./term-syntax.rkt"
           "./contexts.rkt"
           "./test-examples.rkt"
           rackunit
           racket/function
           threading)

  (define-context test-context
    (sort boolean)
    (op true boolean)
    (op false boolean)
    (op (not boolean) boolean)
    (op foo boolean)
    (op bar boolean)
    (=> (not true) false)
    (=> (not false) true)
    (=> foo (not true) #:if false)
    (=> foo (not false) #:if true)
    (eq #:label an-equation
        foo true))

  (define-context test-with-var
    (sort boolean)
    (sort FooBar)
    (op true boolean)
    (op false boolean)
    (op (not boolean) boolean)
    (op foo boolean)
    (op bar boolean)
    (op foo-bar FooBar)
    (var X boolean)
    (=> (not true) false)
    (=> (not false) true)
    (=> (not (not X)) X))

  (define-context test-with-procedure
    (sort boolean)
    (op true boolean)
    (op false boolean)
    (op (not boolean) boolean)
    (var X boolean)
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
(define (test-condition signature rules condition substitution)
  (or (not condition)
      (let* ([s-condition (term.substitute signature condition substitution)]
             [r-condition (reduce signature rules s-condition)])
        (let-values ([(op args) (term.op-and-args r-condition)])
          (and (equal? op 'true)
               (empty? args))))))

(define (in-matching-rules signature rules term test-conditions?)
  (define term-rules (lookup-rules rules term))
  (unless (allowed-term? signature term)
    (error (format "term ~s not allowed by signature\n~s" term signature)))
  (in-generator #:arity 2
   (for* ([rule term-rules]
          [s (term.match signature (rule-pattern rule) term)])
     (when (or (not test-conditions?)
               (test-condition signature rules (rule-condition rule) s))
       (yield rule s)))))

(define (all-matching-rules signature rules term test-conditions?)
  (for/list ([(rule substitution)
              (in-matching-rules signature rules term test-conditions?)])
    (cons rule substitution)))

(module+ test
  (with-context test-context
    (define signature (context-signature test-context))
    (define rules (context-rules test-context))
    (check-equal? (all-matching-rules signature rules (T (not true)) #f)
                  (list (cons (make-rule signature (T (not true)) #f (T false) #f #t)
                              empty-substitution)))
    (check-equal? (all-matching-rules signature rules (T foo) #f)
                  (list (cons (make-rule signature
                                         (T foo) (T false) (T (not true)) #f #t)
                              empty-substitution)
                        (cons (make-rule signature
                                         (T foo) (T true) (T (not false)) #f #t)
                              empty-substitution)))
    (check-equal? (all-matching-rules signature rules (T foo) #t)
                  (list (cons (make-rule signature
                                         (T foo) (T true) (T (not false)) #f #t)
                              empty-substitution)))))

(define (apply-substitution signature rule substitution)
  (define replacement (rule-replacement rule))
  (if (procedure? replacement)
      (with-handlers ([exn:fail? (lambda (v) #f)])
        (replacement signature
                     (rule-pattern rule)
                     (rule-condition rule)
                     substitution))
      (term.substitute signature replacement substitution)))

(define (maybe-result term-or-false)
  (if term-or-false
      (in-value term-or-false)
      empty-sequence))

(define (rewrite-head-once signature rules term)
  (or (for*/first ([(rule substitution) (in-matching-rules signature rules term #t)]
                   [re-term (maybe-result
                             (apply-substitution signature rule substitution))])
        re-term)
      term))

(module+ test

  (with-context test-context
    (define signature (context-signature test-context))
    (define rules (context-rules test-context))
    (check-equal? (rewrite-head-once signature rules (T (not true)))
                  (T false))
    (check-equal? (rewrite-head-once signature rules (T (not false)))
                  (T true))
    (check-equal? (rewrite-head-once signature rules (T (not (not false))))
                  (T (not (not false))))
    (check-equal? (rewrite-head-once signature rules (T foo))
                  (T (not false)))
    (check-exn exn:fail? (thunk (rewrite-head-once signature rules (T 42)))))

  (with-context test-with-var
    (define signature (context-signature test-with-var))
    (define rules (context-rules test-with-var))
    (check-equal? (rewrite-head-once signature rules (T (not true)))
                  (T false))
    (check-equal? (rewrite-head-once signature rules (T (not false)))
                  (T true))
    (check-equal? (rewrite-head-once signature rules (T (not (not false))))
                  (T false))
    (check-exn exn:fail? (thunk (rewrite-head-once signature rules (T 42)))))

  (with-context test-with-procedure
    (define signature (context-signature test-with-procedure))
    (define rules (context-rules test-with-procedure))
    (check-equal? (rewrite-head-once signature rules (T (not true)))
                  (T false))
    (check-equal? (rewrite-head-once signature rules (T (not false)))
                  (T true))
    (check-equal? (rewrite-head-once signature rules (T (not (not false))))
                  (T false))
    (check-exn exn:fail?
               (thunk (rewrite-head-once signature rules (T 42))))))

;
; Recursive rewriting (one step)
;
(define (rewrite-leftmost-innermost signature rules term)
  (define-values (op args) (term.op-and-args term))
  (if op
      ; op-term or op-pattern: first reduce args
      (let* ([reduced-args (map (λ (arg) (reduce signature rules arg)) args)]
             [with-reduced-args (if (andmap eq? args reduced-args)
                                    ; Optimization for unchanged args, avoids
                                    ; unnecessary term construction. Note the
                                    ; use of eq? for doing a fast check.
                                    term
                                    (make-term* signature op reduced-args))])
        (rewrite-head-once signature rules with-reduced-args))
      ; builtin term type: no args
      (rewrite-head-once signature rules term)))

(module+ test
  (with-context test-context
    (define signature (context-signature test-context))
    (define rules (context-rules test-context))
    (check-equal? (rewrite-leftmost-innermost signature rules (T (not true)))
                  (T false))
    (check-equal? (rewrite-leftmost-innermost signature rules (T (not false)))
                  (T true))
    (check-equal? (rewrite-leftmost-innermost signature rules
                                              (T (not (not false))))
                  (T false))
    (check-equal? (rewrite-leftmost-innermost signature rules (T foo))
                  (T (not false)))))

;
; Recursive rewriting to normal form
;
(define (reduce signature rules term)
  (let loop ([term term])
    (let* ([rewritten-term (rewrite-leftmost-innermost signature rules term)])
      (cond
        ; First a fast eq? check
        [(eq? rewritten-term term)    term]
        ; Next the more general but slow equal? check
        [(equal? rewritten-term term) term]
        ; Not equal: another round of rewriting
        [else                         (loop rewritten-term)]))))

(define (trace-reduce signature rules term)
  (displayln (format "-- ~a" term))
  (define rt
    (let loop ([term term])
      (let* ([rewritten-term (rewrite-leftmost-innermost signature rules term)])
        (displayln (format ".. ~a" rewritten-term))
        (cond
          [(eq? rewritten-term term)    term]
          [(equal? rewritten-term term) term]
          [else                         (loop rewritten-term)]))))
  (displayln (format "-> ~a" rt))
  rt)

(module+ test
  (with-context test-context
    (define signature (context-signature test-context))
    (define rules (context-rules test-context))
    (check-equal? (reduce signature rules (T (not true)))
                  (T false))
    (check-equal? (reduce signature rules (T (not false)))
                  (T true))
    (check-equal? (reduce signature rules (T (not (not false))))
                  (T false))
    (check-equal? (reduce signature rules (T foo))
                  (T true))))

;
; Reduction of equations: reduce left and right
;
(define (reduce-equation signature rules equation [new-label #f])
  (make-equation signature
                 (reduce signature rules (equation-left equation))
                 (equation-condition equation)
                 (reduce signature rules (equation-right equation))
                 new-label))

(module+ test
  (with-context test-context
    (define signature (context-signature test-context))
    (define rules (context-rules test-context))
    (check-equal? (reduce-equation signature rules (eq an-equation))
                  (eq true true))
    (check-equal? (reduce-equation signature rules (eq an-equation) 'new-equation)
                  (eq #:label new-equation true true))))

;
; Transformations of terms and equations
;
(define (check-var-conflicts rule term)
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
      (error (format "Var ~s has sort ~s in transformation but sort ~s in the term to be transformed" (var-name v) (var-sort v) (hash-ref tvars-as-hash (var-name v)))))))

(define (transform* signature rule term)
  (apply-substitution signature rule
                      (one-match signature (rule-pattern rule) term)))

(define (transform signature rules transformation term)
  (define tr-rule (transformation-converted-rule transformation))
  (check-var-conflicts tr-rule term)
  (reduce signature rules
          (transform* signature tr-rule term)))

(define (transform-equation signature rules transformation equation [new-label #f])
  (make-equation signature
                 (transform signature rules transformation (equation-left equation))
                 (equation-condition equation)
                 (transform signature rules transformation (equation-right equation))
                 new-label))

(module+ test
  (with-context test-with-var
    (define transformation (tr #:var (X boolean) X (not X)))
    (define signature (context-signature test-with-var))
    (define rules (context-rules test-with-var))
    (check-equal? (transform signature rules transformation (T foo))
                  (T (not foo)))
    (check-equal? (transform signature rules transformation (T X))
                  (T (not X)))
    (check-equal? (transform signature rules transformation (T (not X)))
                  (T X))
    (check-equal? (transform-equation signature rules transformation
                                      (eq foo bar))
                  (eq (not foo) (not bar)))
    (check-equal? (transform-equation signature rules transformation
                                      (eq #:label original foo bar)
                                      'negated)
                  (eq #:label negated (not foo) (not bar))))
  (with-context test-with-var
    (define transformation (tr #:vars ([% boolean] [Y FooBar]) % Y))
    (define signature (context-signature test-with-var))
    (define rules (context-rules test-with-var))
    (check-exn exn:fail?
               (thunk (transform signature rules transformation
                                 (T #:var (Y boolean) (not Y))))))
  (with-context test-context
    (define transformation (tr #:var (X boolean) X (not X)))
    (define signature (context-signature test-context))
    (define rules (context-rules test-context))
    (check-equal? (transform-equation signature rules transformation
                                      (eq an-equation))
                  (eq false false))))

;
; Substitutions in terms and equations
;
(define (substitute* signature rule term)
  (or (for*/first ([s (term.match signature (rule-pattern rule) term)]
                   [re-term (maybe-result
                             (apply-substitution signature rule s))])
        re-term)
      term))

(define (substitute*-leftmost-innermost signature rule term)
  (define-values (op args) (term.op-and-args term))
  (if op
      (let* ([tr-args (for/list ([arg args])
                        (substitute*-leftmost-innermost
                         signature rule arg))]
             [with-tr-args (make-term* signature op tr-args)])
        (substitute* signature rule with-tr-args))
      (substitute* signature rule term)))

(define (substitute signature rules transformation term)
  (define tr-rule (transformation-converted-rule transformation))
  (check-var-conflicts tr-rule term)
  (reduce signature rules
          (substitute*-leftmost-innermost signature tr-rule term)))

(define (substitute-equation signature rules transformation equation [new-label #f])
  (make-equation signature
                 (substitute signature rules transformation (equation-left equation))
                 (equation-condition equation)
                 (substitute signature rules transformation (equation-right equation))
                 new-label))

(module+ test
  (with-context test-context
    (define signature (context-signature test-context))
    (define rules (context-rules test-context))
    (check-equal? (substitute signature rules
                              (tr bar (not bar))
                              (T (not bar)))
                  (T (not (not bar))))
    (check-equal? (substitute signature rules
                              (tr foo (not foo))
                              (T (not foo)))
                  (T true))
    (check-equal? (substitute-equation signature rules
                                       (tr foo (not bar))
                                       (eq foo bar))
                  (eq (not bar) bar))))
