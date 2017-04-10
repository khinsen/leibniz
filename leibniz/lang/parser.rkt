#lang racket

(provide (all-defined-out))

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

(define identifier/p
  (do [first <- letter-or-symbol/p]
      [rest <-  (many/p letter-or-symbol-or-digit/p)]
      (pure (string->symbol (apply string (append (list first) rest))))))

(define reserved-identifiers (set '⊆ '↣ '⇒ '= '∀ 'if))

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
  (check-parse identifier/p "x1" 'x1)
  (check-parse identifier/p "=" '=)
  (check-parse-failure identifier/p "1x")
  (check-parse non-reserved-identifier/p "abc" 'abc)
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
  (or/p (do (char/p #\-)
            [v <- nn-integer/p]
            (pure  (list 'integer (- v))))
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
                  (pure `(term/var ,op-id))))))

(define non-infix-term/p
  (do [t <- simple-term/p]
      (or/p (try/p (or/p (do (char/p #\[)
                             [args <- (many+/p term/p #:sep comma-with-whitespace/p)]
                             (char/p #\])
                             (pure `(term |[]| ,(cons t args))))
                         (do (char/p #\_)
                             (char/p #\{)
                             [subscript <- term/p]
                             (char/p #\})
                             (pure `(term _ (,t ,subscript))))
                         (do (char/p #\^)
                             (char/p #\{)
                             [superscript <- term/p]
                             (char/p #\})
                             (pure `(term ^ (,t ,superscript))))))
            (pure t))))

(define term/p
  (do [t1 <- non-infix-term/p]
      (or/p (try/p (do (many+/p space/p)
                       [op-id <- op-identifier/p]
                       (many+/p space/p)
                       [t2 <- term/p]
                       (pure `(term ,(mark-as-infix op-id) (,t1 ,t2)))))
            (pure t1))))

(module+ test
  (check-parse term/p "foo" '(term/var foo))
  (check-parse term/p "foo(bar,baz)" '(term foo ((term/var bar) (term/var baz))))
  (check-parse term/p "foo(bar, baz)" '(term foo ((term/var bar) (term/var baz))))
  (check-parse term/p "foo(bar , baz)" '(term foo ((term/var bar) (term/var baz))))
  (check-parse term/p "foo[bar]" '(term |[]| ((term/var foo) (term/var bar))))
  (check-parse term/p "foo[bar, baz]" '(term |[]| ((term/var foo) (term/var bar) (term/var baz))))
  (check-parse term/p "foo_{bar}" '(term _ ((term/var foo) (term/var bar))))
  (check-parse term/p "foo^{bar}" '(term ^ ((term/var foo) (term/var bar))))
  (check-parse term/p "bar + baz" '(term _+ ((term/var bar) (term/var baz))))
  (check-parse term/p "foo + bar + baz" '(term _+ ((term/var foo) (term _+ ((term/var bar) (term/var baz))))))
  (check-parse term/p "foo + (bar + baz)" '(term _+ ((term/var foo) (term _+ ((term/var bar) (term/var baz))))))
  (check-parse term/p "(foo + bar) + baz" '(term _+ ((term _+ ((term/var foo) (term/var bar))) (term/var baz) )))
  (check-parse term/p "(foo + 2) - 3⁄4" '(term _- ((term _+ ((term/var foo) (integer 2))) (rational 3/4)))))

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

(define rule/p
  (do [pattern <- term/p]
      (many+/p space/p)
      (char/p #\⇒)
      (many+/p space/p)
      [replacement <- term/p]
      [clauses <- (many/p clause/p)]
      eof/p
      (pure `(rule ,pattern ,replacement ,clauses))))

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
               '(rule (term/var a)
                      (term/var b)
                      ()))
  (check-parse rule/p "a ⇒ b ∀ foo:bar"
               '(rule (term/var a)
                      (term/var b)
                      ((var foo bar))))
  (check-parse rule/p "a ⇒ b ∀ foo:bar ∀ bar:baz"
               '(rule (term/var a)
                      (term/var b)
                      ((var foo bar) (var bar baz))))
  (check-parse rule/p "a ⇒ b ∀ foo:bar if a > 0"
               '(rule (term/var a)
                      (term/var b)
                      ((var foo bar) (term _> ((term/var a) (integer 0))))))
  (check-parse rule/p "a ⇒ b if a > 0"
               '(rule (term/var a)
                      (term/var b)
                      ((term _> ((term/var a) (integer 0))))))
  (check-parse equation/p "a = b ∀ foo:bar ∀ bar:baz"
               '(equation (term/var a)
                          (term/var b)
                          ((var foo bar) (var bar baz))))
  (check-parse equation/p "a = b ∀ foo:bar ∀ bar:baz if test(a)"
               '(equation (term/var a)
                          (term/var b)
                          ((var foo bar) (var bar baz) (term test ((term/var a)))))))


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
