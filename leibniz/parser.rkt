#lang racket

(provide lex-leibniz
         to-eof/p
         sort-or-subsort/p var/p operator/p
         term/p
         rule/p equation/p transformation/p)

(require parser-tools/lex (prefix-in : parser-tools/lex-sre)
         megaparsack megaparsack/parser-tools/lex
         data/monad
         data/applicative)

(module+ test
  (require rackunit
           data/either)
  (define-syntax-rule (check-parse parser string result)
    (check-equal? (parse-tokens (to-eof/p parser) (lex-leibniz string)) (success result)))
  (define-syntax-rule (check-parse-failure parser string)
    (check-true (failure? (parse-tokens (to-eof/p parser) (lex-leibniz string)))))
  (define-syntax-rule (check-lex-failure string)
    (check-exn exn:fail? (λ () (lex-leibniz string)))))

;;
;; First, a lexer that interprets Leibniz expressions as sequences of tokens
;;

(define-tokens leibniz [IDENTIFIER INTEGER RATIONAL FLOAT STRING])
(define-empty-tokens leibniz* [OPEN-PAREN CLOSE-PAREN
                               OPEN-BRACKET CLOSE-BRACKET
                               OPEN-BRACE CLOSE-BRACE
                               SUBSORT COMMA COLON UNDERSCORE HAT
                               EQUAL TRANSFORM-TO REWRITE-TO FORALL IF])

(define-lex-abbrev
  leibniz-id-chars (:or alphabetic
                        #\- #\* #\% #\?
                        (:& symbolic (:~ #\^))))

(define leibniz-lexer
  (lexer-src-pos
   [#\⊆ (token-SUBSORT)]
   [#\( (token-OPEN-PAREN)]
   [#\) (token-CLOSE-PAREN)]
   [#\[ (token-OPEN-BRACKET)]
   [#\] (token-CLOSE-BRACKET)]
   [#\{ (token-OPEN-BRACE)]
   [#\} (token-CLOSE-BRACE)]
   [#\, (token-COMMA)]
   [#\: (token-COLON)]
   [#\_ (token-UNDERSCORE)]
   [#\^ (token-HAT)]
   [#\= (token-EQUAL)]
   [#\⇒ (token-REWRITE-TO)]
   [#\→ (token-TRANSFORM-TO)]
   [#\∀ (token-FORALL)]
   ["if" (token-IF)]
   [(:: (:? #\-) (:+ numeric) #\/ (:+ numeric))
    (token-RATIONAL (string->number lexeme))]
   [(:: (:? #\-) (:+ numeric) #\. (:* numeric) (:? (:: #\e (:? #\-) (:+ numeric))))
    (token-FLOAT (string->number lexeme))]
   [(:: (:? #\-) (:+ numeric))
    (token-INTEGER (string->number lexeme))]
   [(:: #\" (:* (:~ #\")) #\")
    (token-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
   [(:: leibniz-id-chars
       (:* (:or leibniz-id-chars numeric #\.)))
    (token-IDENTIFIER (string->symbol lexeme))]
   [(union whitespace blank iso-control) (void)]
   [(eof) eof]))

(define (lex-leibniz str)
  (define in (open-input-string str))
  (port-count-lines! in)
  (let loop ([v (leibniz-lexer in)])
    (cond [(void? (position-token-token v)) (loop (leibniz-lexer in))]
          [(eof-object? (position-token-token v)) '()]
          [else (cons v (loop (leibniz-lexer in)))])))


;;
;; Next, the parsers that interpret token streams
;;

(define (to-eof/p parser)
  (do [x <- parser]
      eof/p
      (pure x)))

(define identifier/p (token/p 'IDENTIFIER))
(define less-than/p (token/p 'SUBSORT))
(define colon/p (token/p 'COLON))
(define comma/p (token/p 'COMMA))
(define underscore/p (token/p 'UNDERSCORE))
(define hat/p (token/p 'HAT))
(define open-paren/p (token/p 'OPEN-PAREN))
(define close-paren/p (token/p 'CLOSE-PAREN))
(define open-bracket/p (token/p 'OPEN-BRACKET))
(define close-bracket/p (token/p 'CLOSE-BRACKET))
(define open-brace/p (token/p 'OPEN-BRACE))
(define close-brace/p (token/p 'CLOSE-BRACE))

(module+ test
  (check-parse identifier/p "abc" 'abc)
  (check-parse identifier/p "a.b" 'a.b)
  (check-parse identifier/p "ab." 'ab.)
  (check-parse identifier/p "x1" 'x1)
  (check-parse-failure identifier/p "1x")
  (check-lex-failure ".x"))


(define sort-or-subsort/p
  (do [sort1 <- identifier/p]
      (or/p (do less-than/p
                [sort2 <- identifier/p]
                eof/p
                (pure `(subsort ,sort1 ,sort2)))
            (do eof/p
                (pure `(sort ,sort1))))))

(module+ test
  (check-parse sort-or-subsort/p "foo" '(sort foo))
  (check-parse sort-or-subsort/p "foo ⊆ bar" '(subsort foo bar))
  (check-parse sort-or-subsort/p "foo⊆bar" '(sort foo⊆bar))
  (check-parse-failure sort-or-subsort/p "foo⊆ bar"))

(define var/p
  (do [name <- identifier/p]
      colon/p
      [sort <- identifier/p]
      (pure `(var ,name ,sort))))

(module+ test
  (check-parse var/p "foo:bar" '(var foo bar))
  (check-parse var/p "foo: bar" '(var foo bar))
  (check-parse var/p "foo :bar" '(var foo bar))
  (check-parse var/p "foo : bar" '(var foo bar)))

(define (mark-as-infix op-symbol)
  (string->symbol (format "_~a" op-symbol)))

(define var-or-sort/p
  (do [id1 <- identifier/p]
      (or/p (try/p (do colon/p
                       [id2 <- identifier/p]
                       (pure `(var ,id1 ,id2))))
            (pure `(sort ,id1)))))

(define var-in-parens/p
  (do open-paren/p
      [var-id <- identifier/p]
      colon/p
      [sort-id <- identifier/p]
      close-paren/p
      (pure `(var ,var-id ,sort-id))))

(define var-in-parens-or-sort/p
  (or/p (try/p var-in-parens/p)
        (do [sort-id <- identifier/p]
            (pure `(sort ,sort-id)))))

(define (operator-after-first-arg/p arg1)
  (or/p (do open-bracket/p
            [args <- (many+/p var-or-sort/p #:sep comma/p)]
            close-bracket/p
            colon/p
            [result-id <- identifier/p]
            eof/p
            (pure `(op |[]| ,(cons arg1 args) ,result-id)))
        (do underscore/p
            open-brace/p
            [arg2 <- var-or-sort/p]
            close-brace/p
            colon/p
            [result-id <- identifier/p]
            eof/p
            (pure `(op _ (,arg1 ,arg2) ,result-id)))
        (do hat/p
            open-brace/p
            [arg2 <- var-or-sort/p]
            close-brace/p
            colon/p
            [result-id <- identifier/p]
            eof/p
            (pure `(op ^ (,arg1 ,arg2) ,result-id)))
        (do [op-id <- identifier/p]
            [arg2 <- var-in-parens-or-sort/p]
            colon/p
            [result-id <- identifier/p]
            eof/p
            (pure `(op ,(mark-as-infix op-id)
                       (,arg1 ,arg2)
                       ,result-id)))))

(define operator/p
  (or/p (try/p (do [arg1 <- var-in-parens/p]
                   (operator-after-first-arg/p arg1)))
        (do [id1 <- identifier/p] ; sort or op, depending on what follows
            (or/p (try/p (do open-paren/p
                             [args <- (many+/p var-or-sort/p #:sep comma/p)]
                           close-paren/p
                           colon/p
                           [result-id <- identifier/p]
                           eof/p
                           (pure `(op ,id1 ,args ,result-id))))
                  (try/p (do colon/p
                             [result-id <- identifier/p]
                           eof/p
                           (pure `(op ,id1 () ,result-id))))
                  (do [arg1 <- (pure `(sort ,id1))]
                      (operator-after-first-arg/p arg1))))))

(module+ test
  (check-parse operator/p "foo : bar" '(op foo () bar))
  (check-parse operator/p "foo :bar" '(op foo () bar))
  (check-parse operator/p "foo: bar" '(op foo () bar))
  (check-parse operator/p "foo:bar" '(op foo () bar))

  (check-parse operator/p "foo(baz) : bar" '(op foo ((sort baz)) bar))
  (check-parse operator/p "foo( baz) : bar" '(op foo ((sort baz)) bar))
  (check-parse operator/p "foo(baz ) : bar" '(op foo ((sort baz)) bar))
  (check-parse operator/p "foo(baz:bar) : bar" '(op foo ((var baz bar)) bar))
  (check-parse operator/p "foo(baz): bar" '(op foo ((sort baz)) bar))
  (check-parse operator/p "foo(baz:bar) :bar" '(op foo ((var baz bar)) bar))
  (check-parse operator/p "foo(baz):bar" '(op foo ((sort baz)) bar))

  (check-parse operator/p "foo(baz1:bar, baz2) : bar"
               '(op foo ((var baz1 bar) (sort baz2)) bar))
  (check-parse operator/p "foo(baz1:bar ,baz2) : bar"
               '(op foo ((var baz1 bar) (sort baz2)) bar))
  (check-parse operator/p "foo(baz1:bar,baz2) : bar"
               '(op foo ((var baz1 bar) (sort baz2)) bar))
  (check-parse operator/p "foo( baz1:bar, baz2 ) : bar"
               '(op foo ((var baz1 bar) (sort baz2)) bar))
  (check-parse operator/p "foo(baz1:bar,baz2):bar"
               '(op foo ((var baz1 bar) (sort baz2)) bar))
  (check-parse operator/p "foo(baz1:bar, baz2): bar"
               '(op foo ((var baz1 bar) (sort baz2)) bar))
  (check-parse operator/p "foo(baz1:bar , baz2) :bar"
               '(op foo ((var baz1 bar) (sort baz2)) bar))

  (check-parse operator/p "foo[baz1, baz2] : bar"
               '(op |[]| ((sort foo) (sort baz1) (sort baz2)) bar))
  (check-parse operator/p "(foo:bar)[baz1:bar, baz2:bar] : bar"
               '(op |[]| ((var foo bar) (var baz1 bar) (var baz2 bar)) bar))
  (check-parse operator/p "baz1_{baz2} : bar"
               '(op _ ((sort baz1) (sort baz2)) bar))
  (check-parse operator/p "(baz1:bar)_{baz2:bar} : bar"
               '(op _ ((var baz1 bar) (var baz2 bar)) bar))
  (check-parse operator/p "baz1^{baz2} : bar"
               '(op ^ ((sort baz1) (sort baz2)) bar))
  (check-parse operator/p "(baz1:bar)^{baz2:bar} : bar"
               '(op ^ ((var baz1 bar) (var baz2 bar)) bar))

  (check-parse operator/p "baz1 foo baz2 : bar"
               '(op _foo ((sort baz1) (sort baz2)) bar))
  (check-parse operator/p "(baz1:bar) foo (baz2:bar) : bar"
               '(op _foo ((var baz1 bar) (var baz2 bar)) bar))
  (check-parse-failure operator/p "baz1:bar foo baz2 : bar"))


(define integer/p
  (do [x <- (token/p 'INTEGER)]
      (pure `(integer ,x))))

(define rational/p
  (or/p (try/p integer/p)
        (do [x <- (token/p 'RATIONAL)]
            (pure `(rational ,x)))))

(define floating-point/p
  (do [x <- (token/p 'FLOAT)]
      (pure `(floating-point ,x))))

(define number/p
  (or/p floating-point/p
        rational/p))

(module+ test
  (check-parse integer/p "0" '(integer 0))
  (check-parse integer/p "-0" '(integer 0))
  (check-parse integer/p "123" '(integer 123))
  (check-parse integer/p "-123" '(integer -123))
  (check-parse-failure integer/p "abc")
  (check-parse-failure integer/p "")
  (check-parse rational/p "0" '(integer 0))
  (check-parse rational/p "-0" '(integer 0))
  (check-parse rational/p "123" '(integer 123))
  (check-parse rational/p "-123" '(integer -123))
  (check-parse rational/p "2/3" '(rational 2/3))
  (check-parse rational/p "-2/3" '(rational -2/3))
  (check-parse-failure rational/p "2⁄0")
  (check-parse floating-point/p "2.5" '(floating-point 2.5))
  (check-parse floating-point/p "-2.5" '(floating-point -2.5))
  (check-parse floating-point/p "2.5e2" '(floating-point 2.5e2))
  (check-parse floating-point/p "-2.5e2" '(floating-point -2.5e2))
  (check-parse floating-point/p "2.5e-2" '(floating-point 2.5e-2))
  (check-parse floating-point/p "-2.5e-2" '(floating-point -2.5e-2))
  (check-lex-failure ".5")
  (check-parse-failure floating-point/p "+1.5")
  (check-parse number/p "42" '(integer 42))
  (check-parse number/p "42." '(floating-point 42.))
  (check-parse number/p "4.2" '(floating-point 4.2))
  (check-parse number/p "4/2" '(rational 2)))

(define string-literal/p
  (do [s <- (token/p 'STRING)]
      (pure `(string ,s))))

(module+ test
  (check-parse string-literal/p "\"abc\"" '(string "abc"))
  (check-parse string-literal/p "\"\"" '(string "")))


(define simple-term/p
  (or/p (do open-paren/p
            [t <- term/p]
            close-paren/p
            (pure t))
        number/p
        string-literal/p
        (do [op-id <- identifier/p]
            (or/p (do open-paren/p
                      [args <- (many+/p term/p #:sep comma/p)]
                      close-paren/p
                      (pure `(term ,op-id ,args)))
                  (pure `(term-or-var ,op-id))))))

(define simple-term-suffix/p
  (try/p
   (or/p (do underscore/p
             open-brace/p
             [subscripts <- (many+/p term/p #:sep comma/p)]
             close-brace/p
             (pure (cons '_ subscripts)))
         (do hat/p
             open-brace/p
             [superscripts <- (many+/p term/p #:sep comma/p)]
             close-brace/p
             (pure (cons '^ superscripts)))
         (do open-bracket/p
             [args <- (many+/p term/p #:sep comma/p)]
             close-bracket/p
             (pure (cons '|[]| args))))))

(define non-infix-term/p
  (do [t <- simple-term/p]
      [suffix <- (many/p simple-term-suffix/p)]
      (pure (for/fold ([term t])
                      ([s suffix])
              (list 'term (first s) (cons term (rest s)))))))

(define (given-op/p op-id)
  (try/p (guard/p identifier/p
                  (λ (x) (equal? x op-id))
                  (format "infix operator ~a" op-id))))

(define (left-associative infix-op args)
  (case (length args)
    [(0 1) (error "can't happen")]
    [(2) (list 'term infix-op args)]
    [else (let-values ([(left right) (split-at-right args 1)])
            (list 'term infix-op
                  (list  (left-associative infix-op left) (first right))))]))

(define term/p
  (do [first-term <- non-infix-term/p]
      (or/p (try/p (do [op-id <- identifier/p]
                       [more-terms <- (many+/p non-infix-term/p #:sep (given-op/p op-id))]
                       (pure (left-associative (mark-as-infix op-id) (cons first-term more-terms)))))
            (pure first-term))))

(module+ test
  (check-parse term/p "foo" '(term-or-var foo))
  (check-parse term/p "foo(bar,baz)" '(term foo ((term-or-var bar) (term-or-var baz))))
  (check-parse term/p "foo(bar, baz)" '(term foo ((term-or-var bar) (term-or-var baz))))
  (check-parse term/p "foo(bar, \"baz\")" '(term foo ((term-or-var bar) (string "baz"))))
  (check-parse term/p "foo(bar , baz)" '(term foo ((term-or-var bar) (term-or-var baz))))
  (check-parse term/p "foo[bar]" '(term |[]| ((term-or-var foo) (term-or-var bar))))
  (check-parse term/p "foo[bar, baz]" '(term |[]| ((term-or-var foo) (term-or-var bar) (term-or-var baz))))
  (check-parse term/p "foo_{bar}" '(term _ ((term-or-var foo) (term-or-var bar))))
  (check-parse term/p "foo_{bar, baz}" '(term _ ((term-or-var foo) (term-or-var bar) (term-or-var baz))))
  (check-parse term/p "foo^{bar}" '(term ^ ((term-or-var foo) (term-or-var bar))))
  (check-parse term/p "foo^{bar, baz}" '(term ^ ((term-or-var foo) (term-or-var bar) (term-or-var baz))))
  (check-parse term/p "foo_{bar}[baz]" '(term |[]| ((term _ ((term-or-var foo) (term-or-var bar))) (term-or-var baz))))
  (check-parse term/p "foo[bar]_{baz}" '(term _ ((term |[]| ((term-or-var foo) (term-or-var bar))) (term-or-var baz))))
  (check-parse term/p "bar + baz" '(term _+ ((term-or-var bar) (term-or-var baz))))
  (check-parse term/p "foo + bar + baz" '(term _+ ( (term _+ ((term-or-var foo) (term-or-var bar))) (term-or-var baz))))
  (check-parse term/p "a + b + c + d" '(term _+ ((term _+ ((term _+ ((term-or-var a) (term-or-var b))) (term-or-var c))) (term-or-var d))))
  (check-parse term/p "foo + (bar + baz)" '(term _+ ((term-or-var foo) (term _+ ((term-or-var bar) (term-or-var baz))))))
  (check-parse term/p "(foo + bar) + baz" '(term _+ ((term _+ ((term-or-var foo) (term-or-var bar))) (term-or-var baz) )))
  (check-parse term/p "(foo + 2) - 3/4" '(term _- ((term _+ ((term-or-var foo) (integer 2))) (rational 3/4))))
  (check-parse term/p "-(a + b)" '(term - ((term _+ ((term-or-var a) (term-or-var b))))))
  (check-parse-failure term/p "foo + bar × baz"))

(define var-clause/p
  (do (token/p 'FORALL)
      var/p))

(define condition-clause/p
  (do (token/p 'IF)
      term/p))

(define clause/p
  (do (or/p var-clause/p condition-clause/p)))

(define (rule-or-transformation/p separator type-label)
  (do [pattern <- (or/p (try/p var/p) term/p)]
      separator
      [replacement <- term/p]
      [clauses <- (many/p clause/p)]
      eof/p
      (pure (make-rule pattern replacement clauses type-label))))

(define (make-rule pattern replacement clauses type-label)
  (match pattern
    [`(var ,name ,sort)
     `(,type-label (term-or-var ,name) ,replacement ,(cons pattern clauses))]
    [_
     `(,type-label ,pattern ,replacement ,clauses)]))

(define rule/p (rule-or-transformation/p (token/p 'REWRITE-TO) 'rule))

(define transformation/p (rule-or-transformation/p (token/p 'TRANSFORM-TO) 'transformation))

(define equation/p
  (do [left <- term/p]
      (token/p 'EQUAL)
      [right <- term/p]
      [clauses <- (many/p clause/p)]
      eof/p
      (pure `(equation ,left ,right ,clauses))))

(module+ test
  (check-parse var-clause/p "∀ foo:bar" '(var foo bar))
  (check-parse rule/p "a ⇒ b"
               '(rule (term-or-var a)
                      (term-or-var b)
                      ()))
  (check-parse rule/p "a ⇒ b ∀ foo:bar"
               '(rule (term-or-var a)
                      (term-or-var b)
                      ((var foo bar))))
  (check-parse rule/p "a ⇒ b ∀ foo:bar ∀ bar:baz"
               '(rule (term-or-var a)
                      (term-or-var b)
                      ((var foo bar) (var bar baz))))
  (check-parse rule/p "a ⇒ b ∀ foo:bar if a > 0"
               '(rule (term-or-var a)
                      (term-or-var b)
                      ((var foo bar) (term _> ((term-or-var a) (integer 0))))))
  (check-parse rule/p "a ⇒ b if a > 0"
               '(rule (term-or-var a)
                      (term-or-var b)
                      ((term _> ((term-or-var a) (integer 0))))))
  (check-parse rule/p "a ⇒ b + c if a > 0"
               '(rule (term-or-var a)
                      (term _+ ((term-or-var b) (term-or-var c))) 
                      ((term _> ((term-or-var a) (integer 0))))))
  (check-parse transformation/p "a → b + c if a > 0"
               '(transformation (term-or-var a)
                                (term _+ ((term-or-var b) (term-or-var c))) 
                                ((term _> ((term-or-var a) (integer 0))))))
  (check-parse transformation/p "a:foo → b + c if a > 0"
               '(transformation (term-or-var a)
                                (term _+ ((term-or-var b) (term-or-var c))) 
                                ((var a foo) (term _> ((term-or-var a) (integer 0))))))
  (check-parse equation/p "a = b ∀ foo:bar ∀ bar:baz"
               '(equation (term-or-var a)
                          (term-or-var b)
                          ((var foo bar) (var bar baz))))
  (check-parse equation/p "a = b ∀ foo:bar ∀ bar:baz if test(a)"
               '(equation (term-or-var a) 
                         (term-or-var b)
                          ((var foo bar) (var bar baz) (term test ((term-or-var a)))))))
