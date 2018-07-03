#lang racket

(provide
 (contract-out
  [all-matching-rules (signature? rulelist? term? boolean?
                          . -> . (sequence/c (cons/c rule? hash?)))]
  [reduce (signature? rulelist? term? . -> . term?)]
  [in-reduction (signature? rulelist? term? . -> . (sequence/c term?))]
  [trace-reduce ((signature? rulelist? term? procedure?) (integer?)
                            . ->* . term?)]
  [reduce-equation (signature? rulelist? equation? . -> . equation?)]
  [transform (signature? transformation? term? . -> . term?)]
  [transform-equation (signature? transformation? equation?
                       . -> . equation?)]
  [substitute (signature? transformation? term? . -> . term?)]
  [substitute-equation (signature? transformation? equation?
                        . -> . equation?)]))

(require "./sorts.rkt"
         "./operators.rkt"
         "./terms.rkt"
         "./builtins.rkt"
         "./equations.rkt"
         racket/generator
         racket/trace)

(module+ test
  (require (prefix-in ss: "./signature-syntax.rkt")
           "./rule-syntax.rkt"
           "./test-examples.rkt"
           rackunit
           racket/function
           threading)

  (define test-signature
    (ss:signature
     (sort boolean)
     (op true boolean)
     (op false boolean)
     (op (not boolean) boolean)
     (op foo boolean)
     (op bar boolean)))

  (define test-rules
    (rules test-signature
           (=> (not true) false)
           (=> (not false) true)
           (=> foo (not true) #:if false)
           (=> foo (not false) #:if true)))

  (define test-with-var-signature
    (ss:signature
     (sort boolean)
     (sort FooBar)
     (op true boolean)
     (op false boolean)
     (op (not boolean) boolean)
     (op foo boolean)
     (op bar boolean)
     (op foo-bar FooBar)
     (var X boolean)))

  (define test-with-var-rules
    (rules test-with-var-signature
           (=> (not true) false)
           (=> (not false) true)
           (=> (not (not X)) X)))

  (define test-with-procedure-signature
    (ss:signature
     (sort boolean)
     (op true boolean)
     (op false boolean)
     (op (not boolean) boolean)
     (var X boolean)))

  (define test-with-procedure-rules
    (rules test-with-procedure-signature
           (-> (not X) (λ (signature pattern condition substitution)
                         (let ([x (substitution-value substitution 'X)])
                           (define-values (op args) (term.op-and-args x))
                           (cond
                             [(equal? op 'true)
                              (make-term signature 'false empty)]
                             [(equal? op 'false)
                              (make-term signature 'true empty)]
                             [(equal? op 'not)
                              (first args)])))))))

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
  (with-signature test-signature
    (define signature test-signature)
    (define rules test-rules)
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

  (with-signature test-signature
    (define signature test-signature)
    (define rules test-rules)
    (check-equal? (rewrite-head-once signature rules (T (not true)))
                  (T false))
    (check-equal? (rewrite-head-once signature rules (T (not false)))
                  (T true))
    (check-equal? (rewrite-head-once signature rules (T (not (not false))))
                  (T (not (not false))))
    (check-equal? (rewrite-head-once signature rules (T foo))
                  (T (not false)))
    (check-exn exn:fail? (thunk (rewrite-head-once signature rules (T 42)))))

  (with-signature test-with-var-signature
    (define signature test-with-var-signature)
    (define rules test-with-var-rules)
    (check-equal? (rewrite-head-once signature rules (T (not true)))
                  (T false))
    (check-equal? (rewrite-head-once signature rules (T (not false)))
                  (T true))
    (check-equal? (rewrite-head-once signature rules (T (not (not false))))
                  (T false))
    (check-exn exn:fail? (thunk (rewrite-head-once signature rules (T 42)))))

  (with-signature test-with-procedure-signature
    (define signature test-with-procedure-signature)
    (define rules test-with-procedure-rules)
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
  (with-signature test-signature
    (define signature test-signature)
    (define rules test-rules)
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

(module+ test
  (with-signature test-signature
    (define signature test-signature)
    (define rules test-rules)
    (check-equal? (reduce signature rules (T (not true)))
                  (T false))
    (check-equal? (reduce signature rules (T (not false)))
                  (T true))
    (check-equal? (reduce signature rules (T (not (not false))))
                  (T false))
    (check-equal? (reduce signature rules (T foo))
                  (T true))))

;
; Rewriting with trace generation
;
(define (in-reduction signature rules term)
  (in-generator
   (let loop ([term term])
     (let* ([rewritten-term (rewrite-leftmost-innermost signature rules term)])
       (define no-change? (or (eq? rewritten-term term)
                              (equal? rewritten-term term)))
       (if no-change?
           (void)
           (begin (yield rewritten-term)
                  (loop rewritten-term)))))))

(define (trace-reduce signature rules term callback-procedure [level 0])
  (let loop ([term term])
    (let* ([rewritten-term (trace-rewrite-leftmost-innermost signature rules term
                                                             callback-procedure level)])
      (cond
        [(equal? rewritten-term term) term]
        [else                         (loop rewritten-term)]))))

(define (trace-rewrite-leftmost-innermost signature rules term callback-procedure level)
  (define-values (op args) (term.op-and-args term))
  (if op
      (let* ([reduced-args (map (λ (arg) (trace-reduce signature rules arg
                                                       callback-procedure (+ level 1)))
                                args)]
             [with-reduced-args (make-term* signature op reduced-args)])
        (trace-rewrite-head-once signature rules with-reduced-args
                                 callback-procedure level))
      (trace-rewrite-head-once signature rules term
                               callback-procedure level)))

(define (trace-rewrite-head-once signature rules term callback-procedure level)
  (or (for*/first ([(rule substitution) (in-matching-rules signature rules term #t)]
                   [re-term (maybe-result
                             (apply-substitution signature rule substitution))])
        (callback-procedure level term rule re-term)
        re-term)
      term))

;
; Reduction of equations: reduce left and right
;
(define (reduce-equation signature rules equation)
  (make-equation signature
                 (reduce signature rules (equation-left equation))
                 (equation-condition equation)
                 (reduce signature rules (equation-right equation))))

(module+ test
  (with-signature test-signature
    (define signature test-signature)
    (define rules test-rules)
    (check-equal? (reduce-equation signature rules (eq foo true))
                  (eq true true))
    (check-equal? (reduce-equation signature rules (eq foo true))
                  (eq true true))))

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

(define (transform signature transformation term)
  (define tr-rule (transformation-converted-rule transformation))
  (check-var-conflicts tr-rule term)
  (transform* signature tr-rule term))

(define (transform-equation signature transformation equation)
  (make-equation signature
                 (transform signature transformation (equation-left equation))
                 (equation-condition equation)
                 (transform signature  transformation (equation-right equation))))

(module+ test
  (with-signature test-with-var-signature
    (define signature test-with-var-signature)
    (define transformation (tr #:var (X boolean) X (not X)))
    (check-equal? (transform signature transformation (T foo))
                  (T (not foo)))
    (check-equal? (transform signature transformation (T X))
                  (T (not X)))
    (check-equal? (transform signature transformation (T (not X)))
                  (T (not (not X))))
    (check-equal? (transform-equation signature transformation
                                      (eq foo bar))
                  (eq (not foo) (not bar)))
    (check-equal? (transform-equation signature transformation
                                      (eq foo bar))
                  (eq (not foo) (not bar))))
  (with-signature test-with-var-signature
    (define transformation (tr #:vars ([% boolean] [Y FooBar]) % Y))
    (define signature test-with-var-signature)
    (check-exn exn:fail?
               (thunk (transform signature transformation
                                 (T #:var (Y boolean) (not Y))))))
  (with-signature test-with-var-signature
    (define transformation (tr #:var (X boolean) X (not X)))
    (define signature test-with-var-signature)
    (check-equal? (transform-equation signature transformation
                                      (eq foo true))
                  (eq (not foo) (not true)))))

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

(define (substitute signature transformation term)
  (define tr-rule (transformation-converted-rule transformation))
  (check-var-conflicts tr-rule term)
  (substitute*-leftmost-innermost signature tr-rule term))

(define (substitute-equation signature transformation equation)
  (make-equation signature
                 (substitute signature transformation (equation-left equation))
                 (equation-condition equation)
                 (substitute signature transformation (equation-right equation))))

(module+ test
  (with-signature test-signature
    (define signature test-signature)
    (check-equal? (substitute signature
                              (tr bar (not bar))
                              (T (not bar)))
                  (T (not (not bar))))
    (check-equal? (substitute signature
                              (tr foo (not foo))
                              (T (not foo)))
                  (T (not (not foo))))
    (check-equal? (substitute-equation signature
                                       (tr foo (not bar))
                                       (eq foo bar))
                  (eq (not bar) bar))))
