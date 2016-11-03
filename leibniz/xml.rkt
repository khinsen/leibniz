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
            (ops ,@(for/list ([(symbol rank) (all-ops signature)])
                     `(op (@ (id ,(symbol->string symbol)))
                          (arity ,@(for/list ([s (car rank)])
                                     `(sort (@ (id ,(symbol->string s))))))
                          (sort (@ (id ,(symbol->string (cdr rank))))))))
            (rules)
            (equations)))

(module+ test
  (chk #:= (contexts->sxml (hash "truth" truth))
           '(*TOP* (context-collection
                    (context (@ (id "truth"))
                             (sorts (sort (@ (id "Boolean"))))
                             (subsorts)
                             (ops (op (@ (id "true"))
                                      (arity)
                                      (sort (@ (id "Boolean"))))
                                  (op (@ (id "false"))
                                      (arity)
                                      (sort (@ (id "Boolean")))))
                             (rules)
                             (equations))))
       #:= (srl:sxml->xml-noindent (contexts->sxml (hash "truth" truth)))
           "<context-collection><context id=\"truth\"><sorts><sort id=\"Boolean\" /></sorts><subsorts /><ops><op id=\"true\"><arity /><sort id=\"Boolean\" /></op><op id=\"false\"><arity /><sort id=\"Boolean\" /></op></ops><rules /><equations /></context></context-collection>")
  
  (define-context test
    (sort foo)
    (sort bar)
    (subsort foo bar)
    (op (fooify bar) foo))
  (chk #:= (contexts->sxml (hash "test" test))
           '(*TOP* (context-collection
                    (context (@ (id "test"))
                             (sorts (sort (@ (id "bar")))
                                    (sort (@ (id "foo"))))
                             (subsorts (subsort (@ (sort "foo") (sort "bar"))))
                             (ops (op (@ (id "fooify"))
                                      (arity (sort (@ (id "bar"))))
                                      (sort (@ (id "foo")))))
                             (rules)
                             (equations))))))
