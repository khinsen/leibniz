#lang racket

(provide to-eof/p
         sort-or-subsort/p var/p operator/p
         term/p
         rule/p equation/p transformation/p
         parse-scribble-text)

(require megaparsack megaparsack/text
         data/monad
         data/applicative)

(module+ test
  (require rackunit
           data/either)
  (define-syntax-rule (check-parse parser string result)
    (check-equal? (parse-string (to-eof/p parser) string) (success result)))
  (define-syntax-rule (check-parse-failure parser string)
    (check-true (failure? (parse-string (to-eof/p parser) string)))))

(define (to-eof/p parser)
  (do [x <- parser]
      eof/p
      (pure x)))

(define some-punctuation/p
  (label/p "punctuation" (or/p (char/p #\-) (char/p #\*)
                               (char/p #\%) (char/p #\?))))

(define some-symbolic/p
  (label/p "symbolic"
           (try/p (guard/p symbolic/p
                           (λ (x) (not (set-member? (set #\^) x)))
                           "symbolic character"))))

(define letter-or-symbol/p
  (or/p letter/p some-symbolic/p some-punctuation/p ))

(define letter-or-symbol-or-digit/p
  (or/p letter/p some-symbolic/p some-punctuation/p digit/p))

(define letter-or-symbol-or-digit-or-dot/p
  (or/p letter/p some-symbolic/p some-punctuation/p digit/p (char/p #\.)))

(define identifier/p
  (do [first <- letter-or-symbol/p]
      [rest <- (many/p letter-or-symbol-or-digit-or-dot/p)]
      (pure (string->symbol (apply string (append (list first) rest))))))

(define reserved-identifiers (set '⊆ '↣ '→ '⇒ '= '∀ 'if))

(define non-reserved-identifier/p
  (guard/p identifier/p (λ (x) (not (set-member? reserved-identifiers x)))
           "non-reserved identifier"))

(module+ test
  (check-parse letter-or-symbol/p "A" #\A)
  (check-parse letter-or-symbol/p "+" #\+)
  (check-parse letter-or-symbol/p "-" #\-)
  (check-parse letter-or-symbol/p "<" #\<)
  (check-parse letter-or-symbol/p "%" #\%)
  (check-parse letter-or-symbol/p "*" #\*)
  (check-parse-failure letter-or-symbol/p "AB")
  (check-parse-failure letter-or-symbol/p "1")
  (check-parse-failure letter-or-symbol/p ",")
  (check-parse letter-or-symbol-or-digit/p "A" #\A)
  (check-parse letter-or-symbol-or-digit/p "+" #\+)
  (check-parse letter-or-symbol-or-digit/p "-" #\-)
  (check-parse letter-or-symbol-or-digit/p "<" #\<)
  (check-parse letter-or-symbol-or-digit/p "%" #\%)
  (check-parse letter-or-symbol-or-digit/p "1" #\1)
  (check-parse identifier/p "abc" 'abc)
  (check-parse identifier/p "a.b" 'a.b)
  (check-parse identifier/p "ab." 'ab.)
  (check-parse identifier/p "x1" 'x1)
  (check-parse identifier/p "=" '=)
  (check-parse-failure identifier/p "1x")
  (check-parse-failure identifier/p ".x")
  (check-parse non-reserved-identifier/p "abc" 'abc)
  (check-parse non-reserved-identifier/p "a.b" 'a.b)
  (check-parse non-reserved-identifier/p "ab." 'ab.)
  (check-parse non-reserved-identifier/p "x1" 'x1)
  (check-parse-failure non-reserved-identifier/p "="))

(define sort-identifier/p non-reserved-identifier/p)

(define less-than/p
  (do (many+/p space/p)
      (char/p #\⊆)
      (many+/p space/p)))

(define sort-or-subsort/p
  (do [sort1 <- sort-identifier/p]
      (or/p (do less-than/p
                [sort2 <- sort-identifier/p]
                eof/p
                (pure `(subsort ,sort1 ,sort2)))
            (do eof/p
                (pure `(sort ,sort1))))))

(module+ test
  (check-parse sort-or-subsort/p "foo" '(sort foo))
  (check-parse sort-or-subsort/p "foo ⊆ bar" '(subsort foo bar))
  (check-parse sort-or-subsort/p "foo⊆bar" '(sort foo⊆bar))
  (check-parse-failure sort-or-subsort/p "foo⊆ bar"))

(define var-identifier/p non-reserved-identifier/p)

(define colon/p
  (do (char/p #\:)))

(define colon-with-spaces/p
  (do (many+/p space/p)
      (char/p #\:)
      (many+/p space/p)))

(define var/p
  (do [name <- var-identifier/p]
      colon/p
      [sort <- sort-identifier/p]
      (pure `(var ,name ,sort))))

(module+ test
  (check-parse var/p "foo:bar" '(var foo bar))
  (check-parse-failure var/p "foo: bar")
  (check-parse-failure var/p "foo :bar")
  (check-parse-failure var/p "foo : bar"))

(define comma-with-whitespace/p
  (do (many/p space/p)
      (char/p #\,)
      (many/p space/p)))

(define (mark-as-infix op-symbol)
  (string->symbol (format "_~a" op-symbol)))

(define op-identifier/p non-reserved-identifier/p)

(define var-or-sort/p
  (do [id1 <- non-reserved-identifier/p]
      (or/p (try/p (do colon/p
                       [id2 <- sort-identifier/p]
                       (pure `(var ,id1 ,id2))))
            (pure `(sort ,id1)))))

(define operator/p
  (do [id1 <- non-reserved-identifier/p] ; sort or op, depending on what follows
      (or/p (do (char/p #\()
                [args <- (many+/p var-or-sort/p #:sep comma-with-whitespace/p)]
                (char/p #\))
                colon-with-spaces/p
                [result-id <- sort-identifier/p]
                eof/p
                (pure `(op ,id1 ,args ,result-id)))
            (try/p (do colon-with-spaces/p
                       [result-id <- sort-identifier/p]
                       eof/p
                       (pure `(op ,id1 () ,result-id))))
            (do [arg1 <- (or/p (do colon/p
                                   [sort-id <- sort-identifier/p]
                                 (pure`(var ,id1 ,sort-id)))
                               (pure `(sort ,id1)))]
                (or/p (do (char/p #\[)
                          [args <- (many+/p var-or-sort/p #:sep comma-with-whitespace/p)]
                          (char/p #\])
                          colon-with-spaces/p
                          [result-id <- sort-identifier/p]
                          eof/p
                          (pure `(op |[]| ,(cons arg1 args) ,result-id)))
                      (do (char/p #\_)
                          (char/p #\{)
                          [arg2 <- var-or-sort/p]
                          (char/p #\})
                          colon-with-spaces/p
                          [result-id <- sort-identifier/p]
                          eof/p
                          (pure `(op _ (,arg1 ,arg2) ,result-id)))
                      (do (char/p #\^)
                          (char/p #\{)
                          [arg2 <- var-or-sort/p]
                          (char/p #\})
                          colon-with-spaces/p
                          [result-id <- sort-identifier/p]
                          eof/p
                          (pure `(op ^ (,arg1 ,arg2) ,result-id)))
                      (do (many+/p space/p)
                          (do [op-id <- op-identifier/p]
                              (many+/p space/p)
                              [arg2 <- var-or-sort/p]
                              colon-with-spaces/p
                              [result-id <- sort-identifier/p]
                              eof/p
                              (pure `(op ,(mark-as-infix op-id) (,arg1 ,arg2) ,result-id)))))))))

(module+ test
  (check-parse operator/p "foo : bar" '(op foo () bar))
  (check-parse-failure operator/p "foo :bar")
  (check-parse-failure operator/p "foo: bar")
  (check-parse-failure operator/p "foo:bar")

  (check-parse operator/p "foo(baz) : bar" '(op foo ((sort baz)) bar))
  (check-parse operator/p "foo(baz:bar) : bar" '(op foo ((var baz bar)) bar))
  (check-parse-failure operator/p "foo(baz): bar")
  (check-parse-failure operator/p "foo(baz:bar) :bar")
  (check-parse-failure operator/p "foo(baz):bar")

  (check-parse operator/p "foo(baz1:bar, baz2) : bar"
               '(op foo ((var baz1 bar) (sort baz2)) bar))
  (check-parse operator/p "foo(baz1:bar ,baz2) : bar"
               '(op foo ((var baz1 bar) (sort baz2)) bar))
  (check-parse operator/p "foo(baz1:bar,baz2) : bar"
               '(op foo ((var baz1 bar) (sort baz2)) bar))
  (check-parse-failure operator/p "foo(baz1:bar,baz2):bar")
  (check-parse-failure operator/p "foo(baz1:bar, baz2): bar")
  (check-parse-failure operator/p "foo(baz1:bar , baz2) :bar")

  (check-parse operator/p "foo[baz1, baz2] : bar"
               '(op |[]| ((sort foo) (sort baz1) (sort baz2)) bar))
  (check-parse operator/p "foo:bar[baz1:bar, baz2:bar] : bar"
               '(op |[]| ((var foo bar) (var baz1 bar) (var baz2 bar)) bar))
  (check-parse operator/p "baz1_{baz2} : bar"
               '(op _ ((sort baz1) (sort baz2)) bar))
  (check-parse operator/p "baz1:bar_{baz2:bar} : bar"
               '(op _ ((var baz1 bar) (var baz2 bar)) bar))
  (check-parse operator/p "baz1^{baz2} : bar"
               '(op ^ ((sort baz1) (sort baz2)) bar))
  (check-parse operator/p "baz1:bar^{baz2:bar} : bar"
               '(op ^ ((var baz1 bar) (var baz2 bar)) bar))

  (check-parse operator/p "baz1 foo baz2 : bar"
               '(op _foo ((sort baz1) (sort baz2)) bar))
  (check-parse operator/p "baz1:bar foo baz2:bar : bar"
               '(op _foo ((var baz1 bar) (var baz2 bar)) bar))
  (check-parse-failure operator/p "baz1 foo baz2 :bar")
  (check-parse-failure operator/p "baz1 foo baz2 :bar")
  (check-parse-failure operator/p "baz1 foo baz2:bar"))

(define nn-integer/p
  (do [digits <- (many+/p digit/p)]
      (pure (read (open-input-string (apply string digits))))))

(define integer/p
  (or/p (try/p (do (char/p #\-)
                   [v <- nn-integer/p]
                 (pure  (list 'integer (- v)))))
        (do [v <- nn-integer/p]
            (pure (list 'integer v)))))

(define rational/p
  (do [num <- integer/p]
      (or/p (do (or/p (char/p #\/) (char/p #\⁄)) ; Code point 2044 FRACTION SLASH
                [den <- (guard/p nn-integer/p (λ (n) (> n 0)))]
                (pure (list 'rational (/ (second num) den))))
            (pure num))))

(define nn-fp/p
  (do [integer-part <- (many+/p digit/p)]
      (char/p #\.)
      [fractional-part <- (many/p digit/p)]
      [exponent <- (or/p (do (char/p #\e) integer/p)
                         (pure (list 'integer 0)))]
      (pure (read (open-input-string
                   (string-append (apply string (append integer-part (list #\.)
                                                        fractional-part (list #\e) ))
                                  (format "~a" (second exponent))))))))

(define floating-point/p
  (or/p (do (char/p #\-)
            [v <- nn-fp/p]
            (pure  (list 'floating-point (- v))))
        (do [v <- nn-fp/p]
            (pure (list 'floating-point v)))))

(define number/p
  (or/p (try/p floating-point/p)
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
  (check-parse rational/p "2⁄3" '(rational 2/3)) ; fraction slash
  (check-parse rational/p "2/3" '(rational 2/3)) ; standard slash
  (check-parse rational/p "-2⁄3" '(rational -2/3))
  (check-parse-failure rational/p "2⁄0")
  (check-parse floating-point/p "2.5" '(floating-point 2.5))
  (check-parse floating-point/p "-2.5" '(floating-point -2.5))
  (check-parse floating-point/p "2.5e2" '(floating-point 2.5e2))
  (check-parse floating-point/p "-2.5e2" '(floating-point -2.5e2))
  (check-parse floating-point/p "2.5e-2" '(floating-point 2.5e-2))
  (check-parse floating-point/p "-2.5e-2" '(floating-point -2.5e-2))
  (check-parse-failure floating-point/p ".5")
  (check-parse-failure floating-point/p "+1.5")
  (check-parse number/p "42" '(integer 42))
  (check-parse number/p "42." '(floating-point 42.))
  (check-parse number/p "4.2" '(floating-point 4.2))
  (check-parse number/p "4/2" '(rational 2)))

(define simple-term/p
  (or/p (do (char/p #\()
            [t <- term/p]
            (char/p #\))
            (pure t))
        number/p
        (do [op-id <- op-identifier/p]
            (or/p (do (char/p #\()
                      [args <- (many+/p term/p #:sep comma-with-whitespace/p)]
                      (char/p #\))
                      (pure `(term ,op-id ,args)))
                  (pure `(term-or-var ,op-id))))))

(define simple-term-suffix/p
  (try/p
   (or/p (do (char/p #\_)
             (char/p #\{)
             [subscripts <- (many+/p term/p #:sep comma-with-whitespace/p)]
             (char/p #\})
             (pure (cons '_ subscripts)))
         (do (char/p #\^)
             (char/p #\{)
             [superscripts <- (many+/p term/p #:sep comma-with-whitespace/p)]
             (char/p #\})
             (pure (cons '^ superscripts)))
         (do (char/p #\[)
             [args <- (many+/p term/p #:sep comma-with-whitespace/p)]
             (char/p #\])
             (pure (cons '|[]| args))))))

(define non-infix-term/p
  (do [t <- simple-term/p]
      [suffix <- (many/p simple-term-suffix/p)]
      (pure (for/fold ([term t])
                      ([s suffix])
              (list 'term (first s) (cons term (rest s)))))))

(define (given-op/p op-id)
  (try/p (do (many+/p space/p)
             (guard/p op-identifier/p
                      (λ (x) (equal? x op-id))
                      (format "infix operator ~a" op-id))
             (many+/p space/p))))

(define (left-associative infix-op args)
  (case (length args)
    [(0 1) (error "can't happen")]
    [(2) (list 'term infix-op args)]
    [else (let-values ([(left right) (split-at-right args 1)])
            (list 'term infix-op
                  (list  (left-associative infix-op left) (first right))))]))

(define term/p
  (do [first-term <- non-infix-term/p]
      (or/p (try/p (do (many+/p space/p)
                       [op-id <- op-identifier/p]
                       (many+/p space/p)
                       [more-terms <- (many+/p non-infix-term/p #:sep (given-op/p op-id))]
                       (pure (left-associative (mark-as-infix op-id) (cons first-term more-terms)))))
            (pure first-term))))

(module+ test
  (check-parse term/p "foo" '(term-or-var foo))
  (check-parse term/p "foo(bar,baz)" '(term foo ((term-or-var bar) (term-or-var baz))))
  (check-parse term/p "foo(bar, baz)" '(term foo ((term-or-var bar) (term-or-var baz))))
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
  (check-parse term/p "(foo + 2) - 3⁄4" '(term _- ((term _+ ((term-or-var foo) (integer 2))) (rational 3/4))))
  (check-parse term/p "-(a + b)" '(term - ((term _+ ((term-or-var a) (term-or-var b))))))
  (check-parse-failure term/p "foo + bar × baz"))

(define var-clause/p
  (do (char/p #\∀)
      (many+/p space/p)
      var/p))

(define condition-clause/p
  (do (string/p "if")
      (many+/p space/p)
      term/p))

(define clause/p
  (do (many+/p space/p)
      (or/p var-clause/p condition-clause/p)))

(define (rule-or-transformation/p separator type-label)
  (do [pattern <- (or/p (try/p var/p) term/p)]
      (many+/p space/p)
      separator
      (many+/p space/p)
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

(define rule/p (rule-or-transformation/p (char/p #\⇒) 'rule))

(define transformation/p (rule-or-transformation/p (char/p #\→) 'transformation))

(define equation/p
  (do [left <- term/p]
      (many+/p space/p)
      (char/p #\=)
      (many+/p space/p)
      [right <- term/p]
      [clauses <- (many/p clause/p)]
      eof/p
      (pure `(equation ,left ,right ,clauses))))

(module+ test
  (check-parse var-clause/p "∀ foo:bar" '(var foo bar))
  (check-parse-failure var-clause/p "∀ foo : bar")
  (check-parse-failure var-clause/p "∀ foo :bar")
  (check-parse-failure var-clause/p "∀ foo: bar")
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


; Utility function for parsing input from scribble, which may consist of multiple
; string values that may contain newlines.

(define (parse-scribble-text parser text)
  (define syntax-list (syntax->list text))
  (define first-string (first syntax-list))
  (define combined-string
    (if (equal? (length syntax-list) 1)
        first-string
        (datum->syntax first-string
                       (string-normalize-spaces
                        (apply string-append (map syntax->datum syntax-list)))
                       first-string)))
  (parse-result!
   (parse-syntax-string parser combined-string)))
