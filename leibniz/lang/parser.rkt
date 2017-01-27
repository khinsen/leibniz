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
  (label/p "punctuation" (or/p (char/p #\_) (char/p #\-) (char/p #\%))))

(define letter-or-symbol/p
  (or/p letter/p symbolic/p some-punctuation/p ))

(define letter-or-symbol-or-digit/p
  (or/p letter/p symbolic/p some-punctuation/p digit/p))

(define identifier/p
  (do [first <- letter-or-symbol/p]
      [rest <-  (many*/p letter-or-symbol-or-digit/p)]
      (pure (string->symbol (apply string (append (list first) rest))))))

(module+ test
  (check-parse letter-or-symbol/p "A" #\A)
  (check-parse letter-or-symbol/p "+" #\+)
  (check-parse letter-or-symbol/p "-" #\-)
  (check-parse letter-or-symbol/p "<" #\<)
  (check-parse letter-or-symbol/p "%" #\%)
  (check-parse-failure letter-or-symbol/p "AB")
  (check-parse-failure letter-or-symbol/p "1")
  (check-parse-failure letter-or-symbol/p ",")
  (check-parse letter-or-symbol-or-digit/p "A" #\A)
  (check-parse letter-or-symbol-or-digit/p "+" #\+)
  (check-parse letter-or-symbol-or-digit/p "-" #\-)
  (check-parse letter-or-symbol-or-digit/p "<" #\<)
  (check-parse letter-or-symbol-or-digit/p "%" #\%)
  (check-parse letter-or-symbol-or-digit/p "1" #\1))

(define less-than/p
  (do (many+/p space/p)
      (char/p #\<)
      (many+/p space/p)))

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
  (check-parse sort-or-subsort/p "foo < bar" '(subsort foo bar))
  (check-parse sort-or-subsort/p "foo<bar" '(sort foo<bar))
  (check-parse-failure sort-or-subsort/p "foo< bar"))

(define comma-with-whitespace/p
  (do (many*/p space/p)
      (char/p #\,)
      (many*/p space/p)))

(define operator/p
  (do [id1 <- identifier/p]
      (or/p (do (char/p #\()
                [ids <- (many/sep+/p identifier/p comma-with-whitespace/p)]
                (char/p #\))
                (many+/p space/p)
                (char/p #\:)
                (many+/p space/p)
                [result-id <- identifier/p]
                eof/p
                (pure `(prefix-op ,id1 ,ids ,result-id)))
            (do (many+/p space/p)
                (or/p (do [op-id <- identifier/p]
                          (many+/p space/p)
                          [id2 <- identifier/p]
                          (many+/p space/p)
                          (char/p #\:)
                          (many+/p space/p)
                          [result-id <- identifier/p]
                          eof/p
                          (pure `(infix-op ,op-id (,id1 ,id2) ,result-id)))
                      (do (char/p #\:)
                          (many+/p space/p)
                          [result-id <- identifier/p]
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

  (check-parse operator/p "baz1 foo baz2 : bar" '(infix-op foo (baz1 baz2) bar))
  (check-parse-failure operator/p "baz1 foo baz2 :bar")
  (check-parse-failure operator/p "baz1 foo baz2 :bar")
  (check-parse-failure operator/p "baz1 foo baz2:bar"))

(define simple-term/p
  (or/p (do (char/p #\()
            [t <- term/p]
            (char/p #\))
            (pure t))
        (do [op-id <- identifier/p]
            (or/p (do (char/p #\()
                      [args <- (many/sep+/p term/p comma-with-whitespace/p)]
                      (char/p #\))
                      (pure `(term ,op-id ,args)))
                  (pure `(term ,op-id ()))))))

(define term/p
  (do [t1 <- simple-term/p]
      (or/p (try/p (do (many+/p space/p)
                       [op-id <- identifier/p]
                       (many+/p space/p)
                       [t2 <- term/p]
                       (pure `(term ,op-id (,t1 ,t2)))))
            (pure t1))))

(module+ test
  (check-parse term/p "foo" '(term foo ()))
  (check-parse term/p "foo(bar,baz)" '(term foo ((term bar ()) (term baz ()))))
  (check-parse term/p "foo(bar, baz)" '(term foo ((term bar ()) (term baz ()))))
  (check-parse term/p "foo(bar , baz)" '(term foo ((term bar ()) (term baz ()))))
  (check-parse term/p "bar + baz" '(term + ((term bar ()) (term baz ()))))
  (check-parse term/p "foo + bar + baz" '(term + ((term foo ()) (term + ((term bar ()) (term baz ()))))))
  (check-parse term/p "foo + (bar + baz)" '(term + ((term foo ()) (term + ((term bar ()) (term baz ()))))))
  (check-parse term/p "(foo + bar) + baz" '(term + ((term + ((term foo ()) (term bar ()))) (term baz ()) ))))
