#lang racket

(provide (all-from-out scribble/doclang
                       scribble/base)
         empty-document
         import
         context
         parsed-declaration parsed-term parsed-rule parsed-equation parsed-test
         sort op term rule equation
         test
         inset)

(require scribble/doclang
         scribble/base
         scribble/core
         scribble/html-properties
         (for-syntax syntax/parse
                     racket/syntax
                     racket/list
                     "./lang/parser.rkt"
                     megaparsack (except-in megaparsack/text integer/p)
                     data/monad
                     data/applicative)
         "./condd.rkt"
         "./documents.rkt"
         (prefix-in terms: "./terms.rkt")
         (prefix-in operators: "./operators.rkt")
         (prefix-in equations: "./equations.rkt")
         (prefix-in contexts: "./contexts.rkt"))

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
             #:attr decl (list #`(cons (quote parsed) #,(source-loc this-syntax)))
             #:with expansion #`(parsed-rule leibniz-doc current-context 
                                             (quote parsed)
                                             #,(source-loc (first (syntax->list #'(rule-expr ...))))))
    (pattern ((~literal equation) equation-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p equation/p) #'(equation-expr ...))
             #:attr decl empty
             #:with expansion #`(parsed-equation leibniz-doc current-context 
                                                 (quote parsed)
                                                 #,(source-loc (first (syntax->list #'(equation-expr ...))))))
    (pattern ((~literal test) rule-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p rule/p) #'(rule-expr ...))
             #:attr decl empty
             #:with expansion #`(parsed-test leibniz-doc current-context 
                                             (quote parsed)
                                             #,(source-loc (first (syntax->list #'(rule-expr ...))))))
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

; Import library modules

(define-syntax (import stx)
  (syntax-parse stx
    [(_ name:str source:expr)
     (with-syntax ([library-id (format-id #'name "library/~a" (syntax-e #'name))]
                   [leibniz-id (datum->syntax #'name 'leibniz)])
       #'(begin (require (only-in source [leibniz-id library-id]))
                (set! leibniz-id
                      (add-library leibniz-id name library-id))))]))

; Formatting

(define leibniz-css
  (make-css-addition
   #".Leibniz { background-color: #E8E8FF; }\n.LeibnizOutput { background-color: #E0FFE0; }\n"))

(define leibniz-style (style "Leibniz" (list leibniz-css)))
(define leibniz-output-style (style "LeibnizOutput" (list leibniz-css)))

(define (format-sort symbol)
  (elem #:style leibniz-style (italic (symbol->string symbol))))

(define (op-symbol-and-type op-symbol)
  (define s (symbol->string op-symbol))
  (cond
    [(member s '("[]" "^" "_")) (values s 'special-op)]
    [(string-prefix? s "_") (values (substring s 1) 'infix-op)]
    [else (values s 'prefix-op)]))

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
      (define-values (op-str _) (op-symbol-and-type op-symbol))
      (elem #:style leibniz-style
            (format-sort arg-sort-1)
            " " op-str " "
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

(define (term->string term)
  (let ([o (open-output-string)])
    (terms:display-term term o)
    (get-output-string o)))

(define (infix-term? term)
  (define-values (raw-op args) (terms:term.op-and-args term))
  (and raw-op
       (let-values ([(op type) (op-symbol-and-type raw-op)])
         (equal? type 'infix-op))))

(define (parenthesize term-elem condition)
  (if condition
      (list "(" term-elem ")")
      term-elem))

(define (format-term signature term)
  (condd
   [(terms:var? term)                   ; vars: print name in italic
    (italic (symbol->string (terms:var-name term)))]
   #:do (define-values (raw-op args) (terms:term.op-and-args term))
   [(not raw-op)                        ; everything other than an op-term: convert to string
    (term->string term)]
   #:do (define-values (op type) (op-symbol-and-type raw-op))
   [(zero? (length args))               ; no args implies prefix-op
    op]
   [(equal? type 'prefix-op)
    (list op
          "("
          (add-between (for/list ([arg args]) (format-term signature arg)) ", ")
          ")")]
   #:do (define arg1 (first args))
   #:do (define f-arg1 (format-term signature arg1))
   [(equal? op "[]")
    (list (parenthesize f-arg1 (infix-term? arg1))
          "["
          (add-between (for/list ([arg (rest args)]) (format-term signature arg)) ", ")
          "]")]
   #:do (define arg2 (second args))
   #:do (define f-arg2 (format-term signature arg2))
   [(equal? type 'infix-op)
    (list (parenthesize f-arg1 (infix-term? arg1))
          " " op  " " f-arg2)]
   [(equal? op "^")
    (list f-arg1 (superscript f-arg2))]
   [(equal? op "_")
    (list f-arg1 (subscript f-arg2))]
   [else (error (format "operator ~a of type ~a" op type))]))

(define (parsed-term leibniz-doc current-context parsed-term-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (contexts:context-signature context))
  (define term (make-term leibniz-doc current-context parsed-term-expr loc))
  (define sort-str(symbol->string (terms:term.sort term)))
  (define term-elem (format-term signature term))
  (elem #:style (style "Leibniz" (list leibniz-css (hover-property sort-str)))
        term-elem))

(define (parsed-rule leibniz-doc current-context parsed-rule-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (contexts:context-signature context))
  (define rule (make-rule leibniz-doc current-context parsed-rule-expr loc))
  (define vars (terms:term.vars (equations:rule-pattern rule)))
  (define cond (equations:rule-condition rule))
  (define pattern-elem (format-term signature (equations:rule-pattern rule)))
  (define replacement-elem (format-term signature (equations:rule-replacement rule)))
  (define var-elems (for/list ([var (in-set vars)])
                      (elem #:style leibniz-style
                            (linebreak ) (hspace 2)
                            "∀ "
                            (italic (symbol->string (terms:var-name var)))
                            " : "
                            (format-sort (terms:var-sort var)))))
  (define cond-elem
    (if cond
        (elem #:style leibniz-style
              (linebreak) (hspace 2)
              "if "
              (format-term signature cond))
        ""))
  (elem #:style leibniz-style
        pattern-elem
        " ⇒ "
        replacement-elem
        var-elems
        cond-elem))

(define (parsed-equation leibniz-doc current-context parsed-equation-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (contexts:context-signature context))
  (define equation (make-equation leibniz-doc current-context parsed-equation-expr loc))
  (define vars (set-union (terms:term.vars (equations:equation-left equation))
                          (terms:term.vars (equations:equation-right equation))))
  (define cond (equations:equation-condition equation))
  (define left-elem (format-term signature (equations:equation-left equation)))
  (define right-elem (format-term signature (equations:equation-right equation)))
  (define var-elems (for/list ([var (in-set vars)])
                      (elem #:style leibniz-style
                            (linebreak ) (hspace 2)
                            "∀ "
                            (italic (symbol->string (terms:var-name var)))
                            " : "
                            (format-sort (terms:var-sort var)))))
  (define cond-elem
    (if cond
        (elem #:style leibniz-style
              (linebreak) (hspace 2)
              "if "
              (format-term signature cond))
        ""))
  (elem #:style leibniz-style
        left-elem
        " = "
        right-elem
        var-elems
        cond-elem))

(define (parsed-test leibniz-doc current-context parsed-rule-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (contexts:context-signature context))
  (define test (make-test leibniz-doc current-context parsed-rule-expr loc))
  (define success (equal? (second test) (third test)))
  (list
   (elem #:style leibniz-style
         (format-term signature (first test))
         " ⇒ "
         (format-term signature (second test)))
   (nonbreaking (hspace 1))
   (if success
       (elem #:style (style "LeibnizOutput" (list leibniz-css (color-property "green")))
             "✓")
       (elem #:style leibniz-output-style
             (list "❌ " (format-term signature (third test)))))))

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

(define-syntax (test stx)
  (raise-syntax-error #f "test used outside context" stx))

; Support code for nicer formatting

(define (inset . body)
  (apply nested
         (for/list ([element body])
           (if (equal? element "\n")
               (linebreak)
               element))
         #:style 'inset))
