#lang racket

(provide
 (contract-out
  [real->FP32 (context? . -> . context?)]
  [real->FP64 (context? . -> . context?)]))

(require "./sorts.rkt"
         "./operators.rkt"
         "./terms.rkt"
         "./equations.rkt"
         "./contexts.rkt"
         "./builtin-contexts.rkt")

(module+ test
  (require rackunit
           "./rewrite-syntax.rkt"))

; Conversion of contexts using rational and real numbers to contexts using floating-point
; arithmetic
;
; The principle is to replace all sorts in kind [Real] that are not subsorts of Integer
; by FP32 or FP64. Some precautions must be taken to make this work:
;
;  1. The basic arithmetic (operators and rules) from context
;     real-numbers is excluded in this conversion. Its place is taken by
;     the operators and rules already defined in IEEE-floating-point.
;  2. Integer constants are converted to float if they take the place of a
;     rational value that just happens to be an integer.
;  3. Rewrite rules that replace a float expression by an integer expression
;     are converted by adding an int->float conversion on the replacement term.

(define (real->FP32 context)
  (real->float context 'FP32 'ℤ->FP32))

(define (real->FP64 context)
  (real->float context 'FP64 'ℤ->FP64))

(define (real->float context float-sort from-integer)

  (define real-sorts (set 'ℝ 'ℝnz 'ℝp 'ℝnn
                          'ℚ 'ℚnz 'ℚp 'ℚnn))
  (define (translate-sort sort)
    (if (set-member? real-sorts sort)
        float-sort
        sort))

  (define real-number-ops
    (for/fold ([rnops (hash)])
              ([(symbol rank meta) (all-ops (context-signature real-numbers))])
      (hash-update rnops symbol (λ (rs) (set-add rs rank))
                   (set rank))))

  (define sorts (merge-sort-graphs (context-sort-graph context)
                                   (context-sort-graph IEEE-floating-point-with-conversion)))

  (define signature
    (for/fold ([sig (merge-signatures (context-signature integers)
                                      (context-signature IEEE-floating-point-with-conversion)
                                      sorts)])
              ([(symbol rank meta) (all-ops (context-signature context))]
               ; don't translate arithmetic from the real-number context
               #:unless (set-member? (hash-ref real-number-ops symbol (set))
                                     rank))
      (add-op sig
              symbol
              (map translate-sort (car rank))
              (translate-sort (cdr rank)))))

  (define (translate-op-term op args result-sort)
    (define rank (lookup-op (context-signature context) op
                            (map term.sort args)))
    (define t-args (for/list ([a args]
                              [s (car rank)])
                     (translate-term* a (translate-sort s))))
    (make-term signature op t-args))

  (define (translate-number x result-sort)
    (if (or (set-member? real-sorts (term.sort x))
            (equal? result-sort float-sort))
        (case float-sort
          [(FP32) (real->single-flonum x)]
          [(FP64) (real->double-flonum x)])
        x))

  (define (translate-term* term result-sort)
    (define-values (op args) (term.op-and-args term))
    (cond
      [(not (equal? op #f))
       (translate-op-term op args result-sort)]
      [(number? term)
       (translate-number term result-sort)]
      [(var? term)
       (var sorts (var-name term) (translate-sort (var-sort term)))]
      [else
       (error (format "term of unknown type ~a" term))]))

  (define (translate-term term)
    (translate-term* term (translate-sort (term.sort term))))

  (define vars
    (for/fold ([vars (empty-varset sorts)])
              ([(symbol sort) (all-vars (context-vars context))])
      (add-var vars symbol (translate-sort sort))))

  (define real-number-rules
    (for/set ([r (in-rules (context-rules real-numbers))])
      (in-signature r (context-signature context))))

  (define (translate-rule rule)
    (when (procedure? (rule-replacement rule))
      (error "Cannot translate function rules"))
    (define t-pattern (translate-term (rule-pattern rule)))
    (define t-replacement (translate-term (rule-replacement rule)))
    (define t-replacement-conversion
      (if (and (conforms-to? sorts (term.sort t-pattern) (kind sorts float-sort))
               (conforms-to? sorts (term.sort t-replacement) 'ℤ))
          (make-term signature from-integer (list t-replacement))
          t-replacement))
    (make-rule signature
               t-pattern
               (if (rule-condition rule)
                   (translate-term (rule-condition rule))
                   #f)
               t-replacement-conversion
               (rule-label rule)
               #t))

  (define rules
    (for/fold ([rules (context-rules IEEE-floating-point-with-conversion)])
              ([rule (in-rules (context-rules context))]
               #:unless (set-member? real-number-rules rule))
      (define tr-rule (translate-rule rule))
      (if tr-rule
          (add-rule rules tr-rule)
          rules)))

  (define (translate-equation eq)
    (make-equation signature
                   (translate-term (equation-left eq))
                   (if (equation-condition eq)
                       (translate-term (equation-condition eq))
                       #f)
                   (translate-term (equation-right eq))
                   (equation-label eq)))

  (define equations
    (for/fold ([eqs empty-equationset])
              ([eq (in-equations (context-equations context))])
      (add-equation eqs (translate-equation eq))))

  (builtin-context sorts signature vars rules equations))


(module+ test

  ; Heron's iterative algorithm for computing the square root
  ; of X with tolerance ε.
  (define-context heron
    (include real-numbers)
    (op (heron ℝnn ℝp ℝnn) ℝnn)
    (op (heron ℝnn ℝp) ℝnn)
    ; If no starting guess is given, start from 1.
    (=> #:vars ([X ℝnn] [ε ℝp])
        (heron X ε)
        (heron X ε 1))
    ; If the current approximation is good enough, stop.
    (=> #:vars ([X ℝnn] [ε ℝp] [≈√X ℝnn])
        (heron X ε ≈√X)
        ≈√X
        #:if (_< (abs (_- X (_× ≈√X ≈√X))) ε))
    ; One more iteration.
    (=> #:vars ([X ℝnn] [ε ℝp] [≈√X ℝnn])
        (heron X ε ≈√X)
        (heron X ε (_× 1/2 (_+ ≈√X (_÷ X ≈√X))))))

  (with-context heron
    (check-equal? (RT (heron 2 1/2)) (T 3/2))
    (check-equal? (RT (heron 2 1/10)) (T 17/12))
    (check-equal? (RT (heron 2 1/100)) (T 17/12))
    (check-equal? (RT (heron 2 1/200)) (T 577/408)))

  (define float-heron (real->FP64 heron))
  (with-context float-heron
    (check-equal? (RT (heron 2.0 0.5)) (T 1.5))
    (check-true (<  (abs (- (RT (heron 2.0 0.1)) (sqrt 2)))
                    0.005))
    (check-true (<  (abs (- (RT (heron 2.0 0.01)) (sqrt 2)))
                    0.005))
    (check-true (<  (abs (- (RT (heron 2.0 0.005)) (sqrt 2)))
                    1.e-5))))
