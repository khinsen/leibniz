#lang racket

(provide (all-from-out scribble/doclang
                       scribble/base)
         empty-document
         import
         context show-context
         sort var op term rule equation
         comment-sort comment-op
         test eval-term
         inset
         xml signature-graphs)

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
         "./documents.rkt"
         "./formatting.rkt"
         "./transformations.rkt"
         (prefix-in sorts: "./sorts.rkt")
         (prefix-in operators: "./operators.rkt")
         (prefix-in terms: "./terms.rkt")
         (prefix-in contexts: "./contexts.rkt")
         (prefix-in rewrite: "./rewrite.rkt"))

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
    (pattern ((~literal var) var-decl:str ...)
             #:attr parsed (parse-scribble-text (syntax/p (to-eof/p var/p)) #'(var-decl ...))
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
             #:attr decl (list #`(cons (quote parsed) #,(source-loc this-syntax)))
             #:with expansion #`(parsed-equation leibniz-doc current-context 
                                                 (quote parsed)
                                                 #,(source-loc (first (syntax->list #'(equation-expr ...))))))
    (pattern ((~literal comment-sort) sort-decl:str ...)
             #:attr parsed (parse-scribble-text (syntax/p sort-or-subsort/p) #'(sort-decl ...))
             #:attr decl empty
             #:with expansion  #`(parsed-comment (quote parsed)))
    (pattern ((~literal comment-op) op-decl:str ...)
             #:attr parsed (parse-scribble-text (syntax/p operator/p) #'(op-decl ...))
             #:attr decl empty
             #:with expansion #`(parsed-comment (quote parsed)))
    (pattern ((~literal test) rule-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p rule/p) #'(rule-expr ...))
             #:attr decl empty
             #:with expansion #`(parsed-test leibniz-doc current-context 
                                             (quote parsed)
                                             #,(source-loc (first (syntax->list #'(rule-expr ...))))))
    (pattern ((~literal eval-term) term-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p (to-eof/p term/p))
                                                #'(term-expr ...))
             #:attr decl empty
             #:with expansion #`(parsed-eval-term leibniz-doc current-context
                                                  (quote parsed)
                                                  #,(source-loc
                                                     (first (syntax->list
                                                             #'(term-expr ...))))))
    (pattern ((~literal show-context) name:str)
             #:attr decl empty
             #:with expansion #'(format-context-declarations
                                 (get-context-declarations leibniz-doc name)))
    (pattern ((~literal show-context))
             #:attr decl empty
             #:with expansion #'(format-context-declarations
                                 (get-context-declarations leibniz-doc current-context)))
    (pattern (element args:arg-or-kw-arg ...)
             #:attr decl (apply append (attribute args.decl))
             #:with expansion
                    #`(element #,@(apply append
                                  (map syntax-e (syntax-e #'(args.arg-in-list ...))))))
    (pattern any
             #:attr decl empty
             #:with expansion #'any))

  (define-splicing-syntax-class context-ref
    (pattern (~seq #:insert [name:str tr:expr ...])
             #:attr ref #`(cons '(insert name tr ...) #,(source-loc #'name))
             #:attr verb #'"includes transformed")
    (pattern (~seq #:use name:str)
             #:attr ref #`(cons '(use name) #,(source-loc #'name))
             #:attr verb #'"uses")
    (pattern (~seq #:extend name:str)
             #:attr ref #`(cons '(extend name) #,(source-loc #'name))
             #:attr verb #'"extends")))

(define-syntax (context stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ name:str
          include:context-ref ...
          body:body-item ...)
       #`(begin (set! #,leibniz-ref
                      (new-context-from-source #,leibniz-ref name
                                               (append
                                                (list include.ref ...)
                                                #,(cons #'list (apply append (attribute body.decl))))))
                (margin-note "Context " (italic name)
                             (list (linebreak)
                                   include.verb " " (italic include.name)) ...)
                (let ([leibniz-doc #,leibniz-ref]
                      [current-context name])
                  (list body.expansion ...)))])))

; Import library modules

(define-syntax (import stx)
  (syntax-parse stx
    [(_ name:str source:str)
     (with-syntax ([leibniz-id (datum->syntax #'name 'leibniz)])
       #`(set! leibniz-id (import-xml leibniz-id name source)))]
    [(_ name:str source:expr)
     (with-syntax ([library-id (format-id #'name "library/~a" (syntax-e #'name))]
                   [leibniz-id (datum->syntax #'name 'leibniz)])
       #'(begin (require (only-in source [leibniz-id library-id]))
                (set! leibniz-id
                      (add-to-library leibniz-id name library-id))))]))

(define (parsed-term leibniz-doc current-context parsed-term-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (contexts:context-signature context))
  (define term (make-term leibniz-doc current-context parsed-term-expr loc))
  (define sort-str(sorts:constraint->string (operators:signature-sort-graph signature)
                                            (terms:term.sort term)))
  (define term-elem (format-term signature term))
  (leibniz-input-with-hover sort-str term-elem))

(define (parsed-rule leibniz-doc current-context parsed-rule-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define rule (make-rule leibniz-doc current-context parsed-rule-expr loc))
  (leibniz-input (format-rule rule (contexts:context-signature context))))

(define (parsed-equation leibniz-doc current-context parsed-equation-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define equation (make-equation leibniz-doc current-context parsed-equation-expr loc))
  (leibniz-input (format-equation equation (contexts:context-signature context))))

(define (parsed-test leibniz-doc current-context parsed-rule-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (contexts:context-signature context))
  (define test (make-test leibniz-doc current-context parsed-rule-expr loc))
  (define success (equal? (second test) (third test)))
  (list
   (leibniz-input
    (format-term signature (first test))
    " ⇒ "
    (format-term signature (second test)))
   (nonbreaking (hspace 1))
   (leibniz-output
    (if success
        (elem #:style (style #f (list (color-property "green")))
              "✓")
        (list "❌ " (format-term signature (third test)))))))

(define (parsed-eval-term leibniz-doc current-context parsed-term-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (contexts:context-signature context))
  (define rules (contexts:context-rules context))
  (define term (make-term leibniz-doc current-context parsed-term-expr loc))
  (define sort-str(sorts:constraint->string (operators:signature-sort-graph signature)
                                            (terms:term.sort term)))
  (define term-elem (format-term signature term))
  (define rterm (rewrite:reduce signature rules term))
  (define rsort-str(sorts:constraint->string (operators:signature-sort-graph signature)
                                             (terms:term.sort rterm)))
  (define rterm-elem (format-term signature rterm))
  (list
   
   (leibniz-input-with-hover sort-str term-elem)
   (leibniz-output-with-hover rsort-str " ⇒ " rterm-elem)))

(define (parsed-declaration decl)
  (nonbreaking (leibniz-input (format-declaration decl))))

(define (parsed-comment decl)
  (nonbreaking (leibniz-comment (format-declaration decl))))

; Raise errors when Leibniz code is used outside of a context

(define-syntax (sort stx)
  (raise-syntax-error #f "sort used outside context" stx))

(define-syntax (var stx)
  (raise-syntax-error #f "var used outside context" stx))

(define-syntax (op stx)
  (raise-syntax-error #f "op used outside context" stx))

(define-syntax (term stx)
  (raise-syntax-error #f "term used outside context" stx))

(define-syntax (rule stx)
  (raise-syntax-error #f "rule used outside context" stx))

(define-syntax (equation stx)
  (raise-syntax-error #f "equation used outside context" stx))

(define-syntax (comment-sort stx)
  (raise-syntax-error #f "comment-sort used outside context" stx))

(define-syntax (comment-op stx)
  (raise-syntax-error #f "comment-op used outside context" stx))

(define-syntax (test stx)
  (raise-syntax-error #f "test used outside context" stx))

(define-syntax (eval-term stx)
  (raise-syntax-error #f "eval-term used outside context" stx))

; This syntax transformer is only used outside of a context block.
(define-syntax (show-context stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ name:str)
       #`(format-context-declarations (get-context-declarations #,leibniz-ref name))])))

; Support code for nicer formatting

(define (inset . body)
  (apply nested
         (for/list ([element body])
           (if (equal? element "\n")
               (linebreak)
               element))
         #:style 'inset))

; XML output

(define-syntax (xml stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ filename:str)
       #`(begin (write-xml #,leibniz-ref filename)
                (margin-note (hyperlink filename "XML")))])))

; Graphviz output

(define-syntax (signature-graphs stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ directory:str)
       #`(write-signature-graphs #,leibniz-ref directory)])))
