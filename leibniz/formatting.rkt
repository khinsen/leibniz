#lang racket

(provide error->html syntax-error->html
         leibniz-input leibniz-output
         format-sort format-subsort
         format-op
         format-var
         format-rule format-transformation
         asset->html)

(require "./condd.rkt")

(module+ test
  (require rackunit))

(define (syntax-error->html errno tag text position unexpected expected)
  `(@ (leibniz-syntax-error)
      (span ((class "LeibnizErrorMessage")
             (id ,(format "error~a" errno)))
            ,(format "Syntax error #~a at position ~a in " errno position))
      (span ((class "LeibnizError"))
            ,(format "◊~a{~a}" tag text))
      (span ((class "LeibnizErrorMessage"))
            ,(format "unexpected: '~a', expected: ~a"
                     unexpected (string-join expected)))))

(define (error->html errno message source)
  `(@ (span ((class "LeibnizErrorMessage")
             (id ,(format "error~a" errno)))
            ,(format "Error #~a:" errno))
      (span ((class "LeibnizError"))
            ,(format "~a" source))
      (span ((class "LeibnizErrorMessage"))
            ,message)))

;; Input and output values

(define (leibniz-input . elements)
  `(span ((class "LeibnizInput")) ,@elements))

(define (leibniz-output . elements)
  `(span ((class "LeibnizOutput")) ,@elements))

;; Sorts

(define (format-sort symbol)
  `(i ,(symbol->string symbol)))

(define (format-subsort symbol1 symbol2)
  `(@ (i ,(symbol->string symbol1))
      " ⊆ "
      (i ,(symbol->string symbol2))))

;; Terms

(define (op-string-and-type op-symbol)
  (define s (symbol->string op-symbol))
  (cond
    [(member s '("[]" "^" "_")) (values s 'special-op)]
    [(string-prefix? s "_") (values (substring s 1) 'infix-op)]
    [else (values s 'prefix-op)]))

(define (infix-term? term [neighbor-op #f])
  (and (equal? (first term) 'term)
       (let-values ([(op type) (op-string-and-type (second term))])
         (and (equal? type 'infix-op)
              (not (equal? op neighbor-op))))))

(define (parenthesize term-elem condition)
  (if condition
      `(@ "(" ,term-elem ")")
      term-elem))

(define (flatten xexpr)

  (define (sublist? xexpr)
    (and (list? xexpr)
         (equal? '@ (first xexpr))))

  (define (flatten* items acc)
    (condd
      [(empty? items)
       acc]
      #:do (define f (first items))
      #:do (define rf (flatten* (rest items) acc))
      [(sublist? f)
       (flatten* (rest f) rf)]
      #:do (define ff (if (list? f)
                          (cons (first f) (flatten* (rest f) empty))
                          f))
      [(and (string? ff)
            (list? rf)
            (not (empty? rf))
            (string? (first rf)))
       (cons (string-append ff (first rf)) (rest rf))]
      [else
       (cons ff rf)]))

  (define flattened (flatten* (list xexpr) empty))

  (if (equal? 1 (length flattened))
      (first flattened)
      (cons '@ flattened)))

(define (format-term* term)
  (define tag (first term))
  (case tag
    [(var)
     `(i ,(symbol->string (second term)))]
    [(integer rational floating-point)
     (format "~a" (second term))]
    [(term)
     (define raw-op (second term))
     (define-values (op type) (op-string-and-type raw-op))
     (define args (third term))
     (condd
      [(empty? args)
       op]
      [(equal? type 'prefix-op)
       (list '@
        (format "~a(" op)
        (cons '@ (add-between (map format-term* args) ", "))
        ")")]
      #:do (define arg1 (first args))
      #:do (define f-arg1 (format-term* arg1))
      [(equal? op "[]")
       (list '@
        (parenthesize f-arg1 (infix-term? arg1))
        "["
        (cons '@ (add-between (map format-term* (rest args)) ", "))
        "]")]
      #:do (define arg2 (second args))
      #:do (define f-arg2 (format-term* arg2))
      [(equal? type 'infix-op)
       (list '@
        (parenthesize f-arg1 (infix-term? arg1 op))
        " " op  " "
        (parenthesize f-arg2 (infix-term? arg2)))]
      [(equal? op "^")
       (list '@
        (parenthesize f-arg1 (infix-term? arg1 op))
        `(sup ,f-arg2))]
      [(equal? op "_")
       (list '@
        (parenthesize f-arg1 (infix-term? arg1 op))
        `(sub ,f-arg2))]
      [else
       (error (format "operator ~a of type ~a" op type))])]))

(define (format-term term)
  (flatten (format-term* term)))

(module+ test
  (check-equal? (format-term '(integer 3)) "3")
  (check-equal? (format-term '(rational -3/2)) "-3/2")
  (check-equal? (format-term '(floating-point 1.5)) "1.5")
  (check-equal? (format-term '(term a-foo ())) "a-foo")
  (check-equal? (format-term '(var a-foo)) '(i "a-foo"))
  (check-equal? (format-term* '(term a-foo ((term a-bar ()))))
                '(@ "a-foo(" (@ "a-bar") ")"))
  (check-equal? (format-term '(term a-foo ((term a-bar ()))))
                "a-foo(a-bar)")
  (check-equal? (format-term* '(term a-foo ((term a-bar ()) (term a-baz ()))))
                '(@ "a-foo(" (@ "a-bar" ", " "a-baz") ")"))
  (check-equal? (format-term '(term a-foo ((term a-bar ()) (term a-baz ()))))
                "a-foo(a-bar, a-baz)")
  (check-equal? (format-term* '(term |[]| ((term f ()) (term x ()))))
                '(@ "f" "[" (@ "x") "]"))
  (check-equal? (format-term '(term |[]| ((term f ()) (term x ()))))
                "f[x]")
  (check-equal? (format-term* '(term |[]| ((term _+ ((term a ()) (term b ())))
                                           (term x ()))))
                '(@ (@ "(" (@ "a" " " "+" " " "b") ")") "[" (@ "x") "]"))
  (check-equal? (format-term '(term |[]| ((term _+ ((term a ()) (term b ())))
                                          (term x ()))))
                "(a + b)[x]")
  (check-equal? (format-term* '(term _+ ((term a ()) (term b ()))))
                '(@ "a" " " "+" " " "b"))
  (check-equal? (format-term '(term _+ ((term a ()) (term b ()))))
                "a + b")
  (check-equal? (format-term* '(term _+ ((term a ())
                                         (term _+ ((term b ()) (term c ()))))))
                '(@ "a" " " "+" " " (@ "(" (@ "b" " " "+" " " "c") ")")))
  (check-equal? (format-term '(term _+ ((term a ())
                                        (term _+ ((term b ()) (term c ()))))))
                "a + (b + c)")
  (check-equal? (format-term* '(term _+ ((term _+ ((term a ()) (term b ())))
                                         (term c ()))))
                '(@ (@ "a" " " "+" " " "b") " " "+" " " "c"))
  (check-equal? (format-term '(term _+ ((term _+ ((term a ()) (term b ())))
                                        (term c ()))))
                "a + b + c")
  (check-equal? (format-term* '(term ^ ((term a ()) (term b ()))))
                '(@ "a" (sup "b")))
  (check-equal? (format-term '(term ^ ((term a ()) (term b ()))))
                '(@ "a" (sup "b")))
  (check-equal? (format-term* '(term ^ ((term _+ ((term a ()) (term b ())))
                                        (term c ()))))
                '(@ (@ "(" (@ "a" " " "+" " " "b") ")") (sup "c")))
  (check-equal? (format-term '(term ^ ((term _+ ((term a ()) (term b ())))
                                       (term c ()))))
                '(@  "(a + b)"  (sup "c")))
  (check-equal? (format-term* '(term _ ((term a ()) (term b ()))))
                '(@ "a" (sub "b")))
  (check-equal? (format-term '(term _ ((term a ()) (term b ()))))
                '(@ "a" (sub "b")))
  (check-equal? (format-term* '(term _ ((term _+ ((term a ()) (term b ())))
                                        (term c ()))))
                '(@ (@ "(" (@ "a" " " "+" " " "b") ")") (sub "c")))
  (check-equal? (format-term '(term _ ((term _+ ((term a ()) (term b ())))
                                       (term c ()))))
                '(@  "(a + b)"  (sub "c"))))

;; Operator declarations

(define (format-op* op-decl)
  (match-define (list raw-op arity v-sort) op-decl)
  (define-values (op type) (op-string-and-type raw-op))
  (define (format-arg arg)
    (match arg
      [`(sort ,s)
       (format-sort s)]
      [`(var ,n ,s)
       (format-var n s)]))
  (case type
    [(prefix-op)
     (flatten (list '@
               op
               (if (empty? arity)
                   ""
                   (list '@
                         "("
                         (cons '@ (add-between (map format-arg arity) ", "))
                         ")"))
               " : "
               (format-sort v-sort)))]
    [(infix-op)
     (flatten (list '@
                    (format-arg (first arity))
                    " " op " "
                    (format-arg (second arity))
                    " : "
                    (format-sort v-sort)))]
    [(special-op)
     (case op
       [("[]")
        (flatten (list '@
                       (format-arg (first arity))
                       "["
                       (cons '@ (add-between (map format-arg (rest arity)) ", "))
                       "] : "
                       (format-sort v-sort)))]
       [("^")
        (flatten (list '@
                       (format-arg (first arity))
                       (list 'sup (format-arg (second arity)))
                       " : "
                       (format-sort v-sort)))]
       [("_")
        (flatten (list '@
                       (format-arg (first arity))
                       (list 'sub (format-arg (second arity)))
                       " : "
                       (format-sort v-sort)))])]))

(define (format-op op-decl)
  (flatten (format-op* op-decl)))

(module+ test
  (check-equal? (format-op '(a-foo () foo))
                '(@ "a-foo : " (i "foo")))
  (check-equal? (format-op '(a-foo ((sort bar)) foo))
                '(@ "a-foo(" (i "bar") ") : " (i "foo")))
  (check-equal? (format-op '(foo2bar ((var X foo)) bar))
                '(@ "foo2bar(X:" (i "foo") ") : " (i "bar")))
  (check-equal? (format-op '(_+ ((sort foo) (sort foo)) foo))
                '(@ (i "foo") " + " (i "foo") " : " (i "foo")))
  (check-equal? (format-op '(|[]| ((sort foo) (sort bar) (sort bar)) foo))
                '(@ (i "foo") "[" (i "bar") ", " (i "bar")  "] : " (i "foo")))
  (check-equal? (format-op '(^ ((sort foo) (sort bar)) foo))
                '(@ (i "foo") (sup (i "bar")) " : " (i "foo")))
  (check-equal? (format-op '(_ ((sort foo) (sort bar)) foo))
                '(@ (i "foo") (sub (i "bar")) " : " (i "foo"))))

;; Var declarations

(define (format-var name sort)
  (flatten (list '@ (symbol->string name) ":" (format-sort sort))))

(module+ test
  (check-equal? (format-var 'X 'foo)
                '(@ "X:" (i "foo"))))

;; Rules and transformations

(define (format-rule-or-transformation arrow rule-decl)
  (match-define
    (list (or 'rule 'transformation)
          vars pattern replacement condition)
    rule-decl)
  (flatten
   (list '@
         (format-term pattern)
         arrow
         (format-term replacement)
         (cons '@ (for/list ([(name sort) vars])
                    (list '@ " ∀ " (format-var name sort))))
         (if condition
             (list '@ " if " (format-term condition))
             ""))))

(define (format-rule rule-decl)
  (format-rule-or-transformation " ⇒ " rule-decl))

(define (format-transformation rule-decl)
  (format-rule-or-transformation " → " rule-decl))

;; Assets

(define (asset->html asset)
  (case (first asset)
    [(term)
     (leibniz-input (format-term asset))]
    [(rule)
     (leibniz-input (format-rule asset))]
    [(test-result)
     (match-define `(test-result ,left ,right ,actual ,success?) asset)
     (if success?
         (list '@
               (leibniz-input (format-term left)
                              " ⇒ "
                              (format-term right))
               " "
               (leibniz-output '(span ((style "color:green;")) " ✓" )))
         (list '@
               (leibniz-input (format-term left)
                              " ⇒ "
                              `(span ((style "color:red;"))
                                     (s ,(format-term right))))
               " "
               (leibniz-output `(span ((style "color:green;"))
                                      ,(format-term actual)))))]
    [else
     (error "not yet implemented")]))
