#lang racket

(provide (all-from-out scribble/doclang
                       scribble/base)
         sort op)

(require scribble/doclang
         scribble/base
         (for-syntax syntax/parse
                     megaparsack megaparsack/text
                     data/monad
                     data/applicative))

(begin-for-syntax

  (define letter-or-symbolic/p
    (or/p letter/p symbolic/p (char/p #\_)))

  (define letter-or-symbolic-or-digit/p
    (or/p letter/p symbolic/p (char/p #\_) digit/p))

  (define identifier/p
    (do [first <- letter-or-symbolic/p]
        [rest <-  (many*/p letter-or-symbolic-or-digit/p)]
        (pure (string->symbol (apply string (append (list first) rest))))))

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

  (define comma-with-whitespace/p
    (do (many*/p space/p)
        (char/p #\,)
        (many*/p space/p)))

  (define operator/p
    (do [id1 <- identifier/p]
        (or/p (do (char/p #\()
                  [ids <- (many/sep+/p identifier/p comma-with-whitespace/p)]
                  (char/p #\))
                  (many*/p space/p)
                  (char/p #\:)
                  (many*/p space/p)
                  [result-id <- identifier/p]
                  eof/p
                  (pure `(prefix-op ,id1 ,ids ,result-id)))
              (do (many+/p space/p)
                  (or/p (do [op-id <- identifier/p]
                            (many+/p space/p)
                            [id2 <- identifier/p]
                            (many*/p space/p)
                            (char/p #\:)
                            (many*/p space/p)
                            [result-id <- identifier/p]
                            eof/p
                            (pure `(infix-op ,op-id (,id1 ,id2) ,result-id)))
                        (do (char/p #\:)
                            (many*/p space/p)
                            [result-id <- identifier/p]
                            eof/p
                            (pure `(prefix-op ,id1 () ,result-id))))))))

  (define term/p
    identifier/p)
; ⇼  LEFT RIGHT ARROW WITH DOUBLE VERTICAL STROKE
  )

(define (add-to contexts data)
  (hash-set contexts 'current
            (cons data (hash-ref contexts 'current empty))))

; Sorts

(define-syntax (sort stx)
  (let* ([sort-expr (syntax-parse stx
                      [(_ sort-expr:str) #'sort-expr])]
         [parsed-expr (parse-result!
                       (parse-syntax-string (syntax/p sort-or-subsort/p)
                                            sort-expr))]
         [contexts-ref (datum->syntax stx 'contexts)])
    #`(begin (set! #,contexts-ref (add-to #,contexts-ref (quote #,parsed-expr)))
             (format-sort-expr (quote #,parsed-expr)))))

(define (format-sort symbol)
  (bold (italic (symbol->string symbol))))

(define (format-sort-expr parsed-sort-expr)
  (nonbreaking
   (match parsed-sort-expr
     [(list 'sort sort-symbol)
      (format-sort sort-symbol)]
     [(list 'subsort sort-symbol-1 sort-symbol-2)
      (list (format-sort sort-symbol-1)
            " ⊆ "  ; MEDIUM MATHEMATICAL SPACE ; SUBSET OF OR EQUAL TO
            (format-sort sort-symbol-2))])))

; Operators

(define-syntax (op stx)
  (let* ([op-expr (syntax-parse stx
                    [(_ op-expr:str) #'op-expr])]
         [parsed-expr (parse-result!
                       (parse-syntax-string (syntax/p operator/p)
                                            op-expr))]
         [contexts-ref (datum->syntax stx 'contexts)])
    #`(begin (set! #,contexts-ref (add-to #,contexts-ref (quote #,parsed-expr)))
             (format-op-expr (quote #,parsed-expr)))))

(define (format-prefix-op symbol)
  (italic (symbol->string symbol)))

(define (format-infix-op symbol)
  (italic (symbol->string symbol)))

(define (format-op-expr parsed-op-expr)
  (nonbreaking
   (match parsed-op-expr
     [(list 'prefix-op op-symbol '() result-sort)
      (list (format-prefix-op op-symbol)
            " : "
            (format-sort result-sort))]
     [(list 'prefix-op op-symbol arg-sorts result-sort)
      (list (format-prefix-op op-symbol)
            "("
            (add-between (map format-sort arg-sorts) ", ")
            ") : "
            (format-sort result-sort))]
     [(list 'infix-op op-symbol (list arg-sort-1 arg-sort-2) result-sort)
      (list (format-sort arg-sort-1)
            " "
            (format-infix-op op-symbol)
            " "
            (format-sort arg-sort-2)
            " : "
            (format-sort result-sort))])))

; Terms

(define-syntax (term stx)
  (let* ([term-expr (syntax-parse stx
                      [(_ term-expr:str) #'term-expr])]
         [parsed-expr (parse-result!
                       (parse-syntax-string (syntax/p term/p)
                                            term-expr))])
    #`(format-term-expr (quote #,parsed-expr))))

(define (format-term-expr parsed-term-expr)
  (nonbreaking
   " "))
