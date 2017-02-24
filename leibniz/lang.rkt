#lang racket

(provide (all-from-out scribble/doclang
                       scribble/base)
         context
         parsed-declaration parsed-term parsed-rule parsed-equation
         sort op term rule equation
         empty-document
         inset)

(require scribble/doclang
         scribble/base
         scribble/core
         scribble/html-properties
         (for-syntax syntax/parse
                     racket/list
                     "./lang/parser.rkt"
                     megaparsack (except-in megaparsack/text integer/p)
                     data/monad
                     data/applicative)
         "./documents.rkt"
         (prefix-in terms: "./terms.rkt"))

; Translate the location information from a syntax object into strings and numbers
; that can be used at runtime.

(begin-for-syntax
  (define (source-loc stx)
    (list 'list
          (syntax-source stx)
          (syntax-line stx)
          (+ 1 (syntax-column stx))
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
    (pattern ((~literal sort) sort-decl:str ...)
             #:attr parsed (parse-scribble-text (syntax/p sort-or-subsort/p) #'(sort-decl ...))
             #:attr decl (list #`(cons (quote parsed) #,(source-loc this-syntax)))
             #:with expansion  #`(parsed-declaration (quote parsed)))
    (pattern ((~literal op) op-decl:str ...)
             #:attr parsed (parse-scribble-text (syntax/p operator/p) #'(op-decl ...))
             #:attr decl (list #`(cons (quote parsed) #,(source-loc this-syntax)))
             #:with expansion #`(parsed-declaration (quote parsed)))
    (pattern ((~literal term) term-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p (to-eof/p term/p)) #'(term-expr ...))
             #:attr decl empty
             #:with expansion #`(parsed-term leibniz-doc current-context (quote parsed) #,(source-loc (first (syntax->list #'(term-expr ...))))))
    (pattern ((~literal rule) rule-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p rule/p) #'(rule-expr ...))
             #:attr decl empty
             #:with expansion #`(parsed-rule (quote parsed)))
    (pattern ((~literal equation) equation-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p equation/p) #'(equation-expr ...))
             #:attr decl empty
             #:with expansion #`(parsed-equation (quote parsed)))
    (pattern (element args:arg-or-kw-arg ...)
             #:attr decl (apply append (attribute args.decl))
             #:with expansion
                    #`(element #,@(apply append
                                  (map syntax-e (syntax-e #'(args.arg-in-list ...))))))
    (pattern any
             #:attr decl empty
             #:with expansion #'any))

  (define-splicing-syntax-class context-ref
    (pattern (~seq #:use name:str)
             #:attr ref #`(cons name #,(source-loc #'name)))))

(define-syntax (context stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ name:str include:context-ref ... body:body-item ...)
       #`(begin (set! #,leibniz-ref (add-context #,leibniz-ref name
                                                 (list include.ref ...)
                                                 #,(cons #'list (apply append (attribute body.decl)))))
                (margin-note "Context " (italic name)
                             (list (linebreak)
                                   "uses " (italic include.name)) ...)
                (let ([leibniz-doc #,leibniz-ref]
                      [current-context name])
                  (list body.expansion ...)))])))

; Formatting

(define leibniz-css
  (make-css-addition
   #".Leibniz { background-color: #F0F0FF; }\n.LeibnizSort { background-color: #E0E0FF; }\n"))

(define leibniz-style (style "Leibniz" (list leibniz-css)))
(define sort-style (style "LeibnizSort" (list leibniz-css)))

(define (format-sort symbol)
  (elem #:style sort-style (symbol->string symbol)))

(define (parsed-declaration decl)
  (nonbreaking
   (match decl
     [(list 'sort sort-symbol)
      (format-sort sort-symbol)]
     [(list 'subsort sort-symbol-1 sort-symbol-2)
      (list (format-sort sort-symbol-1)
            (elem #:style leibniz-style " ⊆ ")
            ; MEDIUM MATHEMATICAL SPACE ; SUBSET OF OR EQUAL TO
            (format-sort sort-symbol-2))]
     [(list 'prefix-op op-symbol arg-sorts result-sort)
      (elem #:style leibniz-style
            (symbol->string op-symbol)
            (if (zero? (length arg-sorts))
                ""
                (list "(" (add-between (map format-sort arg-sorts) ", ") ")"))
            " : "
            (format-sort result-sort))]
     [(list 'infix-op op-symbol (list arg-sort-1 arg-sort-2) result-sort)
      (elem #:style leibniz-style
            (format-sort arg-sort-1)
            " "
            (symbol->string op-symbol)
            " "
            (format-sort arg-sort-2)
            " : "
            (format-sort result-sort))]
     [(list 'special-op '|[]| (list f-arg-sort arg-sorts ... ) result-sort)
      (elem #:style leibniz-style
            (format-sort f-arg-sort)
            "["
            (add-between (map format-sort arg-sorts) ", ")
            "] : "
            (format-sort result-sort))]
     [(list 'special-op '_ (list arg-sort-1 arg-sort-2) result-sort)
      (elem #:style leibniz-style
            (format-sort arg-sort-1)
            (subscript (format-sort arg-sort-2))
            " : "
            (format-sort result-sort))]
     [(list 'special-op '^ (list arg-sort-1 arg-sort-2) result-sort)
      (elem #:style leibniz-style
            (format-sort arg-sort-1)
            (superscript (format-sort arg-sort-2))
            " : "
            (format-sort result-sort))])))


(define (parsed-term leibniz-doc current-context parsed-term-expr loc)
  (define term (make-term leibniz-doc current-context parsed-term-expr loc))
  (define term-as-str (let ([o (open-output-string)])
                        (terms:display-term term o)
                        (get-output-string o)))
  (elem #:style (style #f (list (hover-property (symbol->string (terms:term.sort term)))))
        term-as-str))

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


; Support code for nicer formatting

(define (inset . body)
  (apply nested
         (for/list ([element body])
           (if (equal? element "\n")
               (linebreak)
               element))
         #:style 'inset))
