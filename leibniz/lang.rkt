#lang racket

(provide (all-from-out scribble/doclang
                       scribble/base)
         context
         parsed-declaration parsed-term parsed-rule parsed-equation
         sort op term rule equation
         empty-document)

(require scribble/doclang
         scribble/base
         (for-syntax syntax/parse
                     racket/list
                     "./lang/parser.rkt"
                     megaparsack megaparsack/text
                     data/monad
                     data/applicative)
         "./documents.rkt")

; Translate the location information from a syntax object into strings and numbers
; that can be used at runtime.

(begin-for-syntax
  (define (source-loc stx)
    (list 'list
          (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx))))

; Context definitions

; All Leibniz declarations are collected from the body of a context and used to define
; the context inside the single "document" object referred to by "leibniz".

(begin-for-syntax

  (define-splicing-syntax-class arg-or-kw-arg
    (pattern (~seq kw:keyword arg:expr)
             #:with arg-in-list #'(kw arg)
             #:attr decl empty)
    (pattern arg:body-item
             #:with arg-in-list #'(arg.expansion)
             #:attr decl (attribute arg.decl)))

  (define-syntax-class body-item
    (pattern ((~literal sort) sort-decl:str)
             #:attr parsed (parse-result!
                            (parse-syntax-string (syntax/p sort-or-subsort/p)
                                                 #'sort-decl))
             #:attr decl (list #`(cons (quote parsed) #,(source-loc this-syntax)))
             #:with expansion  #`(parsed-declaration (quote parsed)))
    (pattern ((~literal op) op-decl:str)
             #:attr parsed (parse-result!
                            (parse-syntax-string (syntax/p operator/p)
                                                 #'op-decl))
             #:attr decl (list #`(cons (quote parsed) #,(source-loc this-syntax)))
             #:with expansion #`(parsed-declaration (quote parsed)))
    (pattern ((~literal term) term-expr:str)
             #:attr parsed (parse-result!
                            (parse-syntax-string (syntax/p (to-eof/p term/p))
                                                 #'term-expr))
             #:attr decl empty
             #:with expansion #`(parsed-term (quote parsed)))
    (pattern ((~literal rule) rule-expr:str)
             #:attr parsed (parse-result!
                            (parse-syntax-string (syntax/p rule/p)
                                                 #'rule-expr))
             #:attr decl empty
             #:with expansion #`(parsed-rule (quote parsed)))
    (pattern ((~literal equation) equation-expr:str)
             #:attr parsed (parse-result!
                            (parse-syntax-string (syntax/p equation/p)
                                                 #'equation-expr))
             #:attr decl empty
             #:with expansion #`(parsed-equation (quote parsed)))
    (pattern (element args:arg-or-kw-arg ...)
             #:attr decl (apply append (attribute args.decl))
             #:with expansion
                    #`(element #,@(apply append
                                  (map syntax-e (syntax-e #'(args.arg-in-list ...))))))
    (pattern any
             #:attr decl empty
             #:with expansion #'any)))

(define-syntax (context stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ name body:body-item ...)
       #`(begin (set! #,leibniz-ref (add-context #,leibniz-ref name
                                                 #,(cons #'list (apply append (attribute body.decl)))))
                body.expansion ...)])))

; Formatting

(define (parsed-declaration decl)
  (nonbreaking
   (match decl
     [(list 'sort sort-symbol)
      (format-sort sort-symbol)]
     [(list 'subsort sort-symbol-1 sort-symbol-2)
      (list (format-sort sort-symbol-1)
            " ⊆ "  ; MEDIUM MATHEMATICAL SPACE ; SUBSET OF OR EQUAL TO
            (format-sort sort-symbol-2))]
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

(define (format-sort symbol)
  (bold (italic (symbol->string symbol))))

(define (format-prefix-op symbol)
  (italic (symbol->string symbol)))

(define (format-infix-op symbol)
  (italic (symbol->string symbol)))

(define (parsed-term parsed-term-expr)
  (nonbreaking) (format "~a" parsed-term-expr))

(define (parsed-rule parsed-rule-expr)
  (nonbreaking (format "~a" parsed-rule-expr)))

(define (parsed-equation parsed-equation-expr)
  (nonbreaking (format "~a" parsed-equation-expr)))

; Raise errors when Leibniz code is used outside of a context

(define-syntax (sort stx)
  (raise-syntax-error #f "sort used outside context" stx))

(define-syntax (op stx)
  (raise-syntax-error #f "op used outside context" stx))

(define-syntax (term stx)
  (raise-syntax-error #f "term used outside context" stx))

(define-syntax (rule stx)
  (raise-syntax-error #f "rule used outside context" stx))

(define-syntax (equation stx)
  (raise-syntax-error #f "equation used outside context" stx))
