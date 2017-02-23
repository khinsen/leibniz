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
  (label/p "punctuation" (or/p (char/p #\-) (char/p #\*) (char/p #\%))))

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

(define reserved-identifiers (set '⊆ '→ '⇒ '= '∀))

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

(define comma-with-whitespace/p
  (do (many/p space/p)
      (char/p #\,)
      (many/p space/p)))

(define op-identifier/p non-reserved-identifier/p)

(define operator/p
  (do [id1 <- non-reserved-identifier/p] ; sort or op, depending on what follows
      (or/p (do (char/p #\()
                [ids <- (many+/p sort-identifier/p #:sep comma-with-whitespace/p)]
                (char/p #\))
                (many+/p space/p)
                (char/p #\:)
                (many+/p space/p)
                [result-id <- sort-identifier/p]
                eof/p
                (pure `(prefix-op ,id1 ,ids ,result-id)))
            (do (char/p #\[)
                [ids <- (many+/p sort-identifier/p #:sep comma-with-whitespace/p)]
                (char/p #\])
                (many+/p space/p)
                (char/p #\:)
                (many+/p space/p)
                [result-id <- sort-identifier/p]
                eof/p
                (pure `(special-op |[]| ,(cons id1 ids) ,result-id)))
            (do (char/p #\_)
                (char/p #\{)
                [id2 <- sort-identifier/p]
                (char/p #\})
                (many+/p space/p)
                (char/p #\:)
                (many+/p space/p)
                [result-id <- sort-identifier/p]
                eof/p
                (pure `(special-op _ (,id1 ,id2) ,result-id)))
            (do (char/p #\^)
                (char/p #\{)
                [id2 <- sort-identifier/p]
                (char/p #\})
                (many+/p space/p)
                (char/p #\:)
                (many+/p space/p)
                [result-id <- sort-identifier/p]
                eof/p
                (pure `(special-op ^ (,id1 ,id2) ,result-id)))
            (do (many+/p space/p)
                (or/p (do [op-id <- op-identifier/p]
                          (many+/p space/p)
                          [id2 <- sort-identifier/p]
                          (many+/p space/p)
                          (char/p #\:)
                          (many+/p space/p)
                          [result-id <- sort-identifier/p]
                          eof/p
                          (pure `(infix-op ,op-id (,id1 ,id2) ,result-id)))
                      (do (char/p #\:)
                          (many+/p space/p)
                          [result-id <- sort-identifier/p]
                          eof/p
                          (pure `(prefix-op ,id1 () ,result-id))))))))

(module+ test
  (check-parse operator/p "foo : bar" '(prefix-op foo () bar))
  (check-parse-failure operator/p "foo :bar")
  (check-parse-failure operator/p "foo: bar")
  (check-parse-failure operator/p "foo:bar")

  (check-parse operator/p "foo(baz) : bar" '(prefix-op foo (baz) bar))
  (check-parse-failure operator/p "foo(baz): bar")
  (check-parse-failure operator/p "foo(baz) :bar")
  (check-parse-failure operator/p "foo(baz):bar")

  (check-parse operator/p "foo(baz1, baz2) : bar" '(prefix-op foo (baz1 baz2) bar))
  (check-parse operator/p "foo(baz1 ,baz2) : bar" '(prefix-op foo (baz1 baz2) bar))
  (check-parse operator/p "foo(baz1,baz2) : bar" '(prefix-op foo (baz1 baz2) bar))
  (check-parse-failure operator/p "foo(baz1,baz2):bar")
  (check-parse-failure operator/p "foo(baz1, baz2): bar")
  (check-parse-failure operator/p "foo(baz1 , baz2) :bar")

  (check-parse operator/p "foo[baz1, baz2] : bar" '(special-op |[]| (foo baz1 baz2) bar))
  (check-parse operator/p "baz1_{baz2} : bar" '(special-op _ (baz1 baz2) bar))
  (check-parse operator/p "baz1^{baz2} : bar" '(special-op ^ (baz1 baz2) bar))

  (check-parse operator/p "baz1 foo baz2 : bar" '(infix-op foo (baz1 baz2) bar))
  (check-parse-failure operator/p "baz1 foo baz2 :bar")
  (check-parse-failure operator/p "baz1 foo baz2 :bar")
  (check-parse-failure operator/p "baz1 foo baz2:bar"))

(define simple-term/p
  (or/p (do (char/p #\()
            [t <- term/p]
            (char/p #\))
            (pure t))
        (do [op-id <- op-identifier/p]
            (or/p (do (char/p #\()
                      [args <- (many+/p term/p #:sep comma-with-whitespace/p)]
                      (char/p #\))
                      (pure `(term ,op-id ,args)))
                  (pure `(term ,op-id ()))))))

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
                       (pure `(term ,op-id (,t1 ,t2)))))
            (pure t1))))

(module+ test
  (check-parse term/p "foo" '(term foo ()))
  (check-parse term/p "foo(bar,baz)" '(term foo ((term bar ()) (term baz ()))))
  (check-parse term/p "foo(bar, baz)" '(term foo ((term bar ()) (term baz ()))))
  (check-parse term/p "foo(bar , baz)" '(term foo ((term bar ()) (term baz ()))))
  (check-parse term/p "foo[bar]" '(term |[]| ((term foo ()) (term bar ()))))
  (check-parse term/p "foo[bar, baz]" '(term |[]| ((term foo ()) (term bar ()) (term baz ()))))
  (check-parse term/p "foo_{bar}" '(term _ ((term foo ()) (term bar ()))))
  (check-parse term/p "foo^{bar}" '(term ^ ((term foo ()) (term bar ()))))
  (check-parse term/p "bar + baz" '(term + ((term bar ()) (term baz ()))))
  (check-parse term/p "foo + bar + baz" '(term + ((term foo ()) (term + ((term bar ()) (term baz ()))))))
  (check-parse term/p "foo + (bar + baz)" '(term + ((term foo ()) (term + ((term bar ()) (term baz ()))))))
  (check-parse term/p "(foo + bar) + baz" '(term + ((term + ((term foo ()) (term bar ()))) (term baz ()) ))))

(define var/p
  (do (char/p #\∀)
      (many+/p space/p)
      [name <- op-identifier/p]
      (many*/p space/p)
      (char/p #\:)
      (many*/p space/p)
      [sort <- sort-identifier/p]
      (pure `(var ,name ,sort))))

(define var-after-space/p
  (do (many+/p space/p)
      [v <- var/p]
      (pure v)))

(define rule/p
  (do [pattern <- term/p]
      (many+/p space/p)
      (char/p #\⇒)
      (many+/p space/p)
      [replacement <- term/p]
      [vars <- (many/p var-after-space/p)]
      eof/p
      (pure `(rule ,pattern ,replacement ,vars))))

(define equation/p
  (do [left <- term/p]
      (many+/p space/p)
      (char/p #\=)
      (many+/p space/p)
      [right <- term/p]
      [vars <- (many/p var-after-space/p)]
      eof/p
      (pure `(equation ,left ,right ,vars))))

(module+ test
  (check-parse var/p "∀ foo:bar" '(var foo bar))
  (check-parse var/p "∀ foo : bar" '(var foo bar))
  (check-parse var/p "∀ foo :bar" '(var foo bar))
  (check-parse var/p "∀ foo: bar" '(var foo bar))
  (check-parse rule/p "a ⇒ b" '(rule (term a ()) (term b ()) ()))
  (check-parse rule/p "a ⇒ b ∀ foo:bar" '(rule (term a ()) (term b ()) ((var foo bar))))
  (check-parse rule/p "a ⇒ b ∀ foo:bar ∀ bar:baz"
               '(rule (term a ()) (term b ()) ((var foo bar) (var bar baz))))
  (check-parse equation/p "a = b ∀ foo:bar ∀ bar:baz"
               '(equation (term a ()) (term b ()) ((var foo bar) (var bar baz)))))


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
