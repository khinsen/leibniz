#lang racket

(provide contexts->sxml)

(require "./contexts.rkt"
         "./operators.rkt"
         "./sorts.rkt"
         sxml)

(module+ test
  (require chk
           "./builtin-contexts.rkt"))

; A Leibniz XML document is equivalent to a hash mapping strings to contexts.

(define (contexts->sxml contexts)
  `(*TOP* (context-collection
           ,@(for/list ([(name context) contexts])
               (context->sxml name context)))))

(define (context->sxml name context)
  (define signature (context-signature context))
  (define sorts (signature-sort-graph signature))
  `(context (@ (id ,name))
            (sorts ,@(for/list ([s (all-sorts sorts)])
                       `(sort (@ (id ,(symbol->string s))))))
            (subsorts ,@(for/list ([ss (all-subsort-relations sorts)])
                          `(subsort (@ (sort ,(symbol->string (car ss)))
                                       (sort ,(symbol->string (cdr ss)))))))
            (ops ,@(for/list ([(symbol rank meta) (all-ops signature)])
                     `(op (@ (id ,(symbol->string symbol)))
                          (arity ,@(for/list ([s (car rank)])
                                     `(sort (@ (id ,(symbol->string s))))))
                          (sort (@ (id ,(symbol->string (cdr rank))))))))
            (rules)
            (equations)))

(module+ test
  (define truth-sxml-1
    '(*TOP* (context-collection
             (context (@ (id "truth"))
                      (sorts (sort (@ (id "boolean"))))
                      (subsorts)
                      (ops (op (@ (id "true"))
                               (arity)
                               (sort (@ (id "boolean"))))
                           (op (@ (id "false"))
                               (arity)
                               (sort (@ (id "boolean")))))
                      (rules)
                      (equations)))))
  (define truth-sxml-2
    '(*TOP* (context-collection
             (context (@ (id "truth"))
                      (sorts (sort (@ (id "boolean"))))
                      (subsorts)
                      (ops (op (@ (id "false"))
                               (arity)
                               (sort (@ (id "boolean"))))
                           (op (@ (id "true"))
                               (arity)
                               (sort (@ (id "boolean")))))
                      (rules)
                      (equations)))))
  (define truth-xml-1 (srl:sxml->xml-noindent truth-sxml-1))
  (define truth-xml-2 (srl:sxml->xml-noindent truth-sxml-2))
  (chk #:t (set-member? (set truth-sxml-1 truth-sxml-2)
                        (contexts->sxml (hash "truth" truth)))
       #:t (set-member? (set truth-xml-1 truth-xml-2)
                        (srl:sxml->xml-noindent (contexts->sxml (hash "truth" truth)))))
  
  (define-context test
    (sort foo)
    (sort bar)
    (subsort foo bar)
    (op (fooify bar) foo))
  (define test-sxml-1
    '(*TOP* (context-collection
             (context (@ (id "test"))
                      (sorts (sort (@ (id "bar")))
                             (sort (@ (id "foo"))))
                      (subsorts (subsort (@ (sort "foo") (sort "bar"))))
                      (ops (op (@ (id "fooify"))
                               (arity (sort (@ (id "bar"))))
                               (sort (@ (id "foo")))))
                      (rules)
                      (equations)))))
  (define test-sxml-2
    '(*TOP* (context-collection
             (context (@ (id "test"))
                      (sorts (sort (@ (id "foo")))
                             (sort (@ (id "bar"))))
                      (subsorts (subsort (@ (sort "foo") (sort "bar"))))
                      (ops (op (@ (id "fooify"))
                               (arity (sort (@ (id "bar"))))
                               (sort (@ (id "foo")))))
                      (rules)
                      (equations)))))
  
  (chk #:t (set-member? (set test-sxml-1 test-sxml-2)
                        (contexts->sxml (hash "test" test)))))
