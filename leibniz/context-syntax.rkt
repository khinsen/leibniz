#lang racket

(provide context import
         sort var op term rule equation transformation
         comment-sort comment-op
         test eval-term
         ref
         substitute transform
         show-context)

(require "./documents.rkt"
         "./formatting.rkt"
         (prefix-in sorts: "./sorts.rkt")
         (prefix-in operators: "./operators.rkt")
         (prefix-in terms: "./terms.rkt")
         (prefix-in equations: "./equations.rkt")
         (prefix-in rewrite: "./rewrite.rkt")
         scribble/core
         scribble/base
         (for-syntax syntax/parse
                     racket/syntax
                     racket/list
                     "./parser.rkt"
                     megaparsack (except-in megaparsack/text integer/p)
                     data/monad
                     data/applicative))

(module+ test
  (require rackunit
           threading))

;; Translate the location information from a syntax object into strings and numbers
;; that can be used at runtime.
;;
(begin-for-syntax
  (define (source-loc stx)
    (list 'list
          (syntax-source stx)
          (syntax-line stx)
          (+ 1 (syntax-column stx))
          (syntax-position stx)
          (syntax-span stx))))

;;
;; Context definitions
;;
;; All Leibniz declarations are collected from the body of a context and used to define
;; the context inside the single "document" object referred to by "leibniz".
;;
(begin-for-syntax

  (define-splicing-syntax-class arg-or-kw-arg
    (pattern (~seq kw:keyword arg:expr)
             #:with arg-in-list #'(kw arg)
             #:attr decl empty)
    (pattern arg:body-item
             #:with arg-in-list #'(arg.expansion)
             #:attr decl (attribute arg.decl)))

  (define-syntax-class body-item
    ;; Signature declarations
    (pattern ((~literal sort) sort-decl:str ...)
             #:attr parsed (parse-scribble-text (syntax/p sort-or-subsort/p) #'(sort-decl ...))
             #:attr decl (list #`(cons (quote parsed) #,(source-loc this-syntax)))
             #:with expansion  #`(parsed-signature-declaration (quote parsed)))
    (pattern ((~literal var) var-decl:str ...)
             #:attr parsed (parse-scribble-text (syntax/p (to-eof/p var/p)) #'(var-decl ...))
             #:attr decl (list #`(cons (quote parsed) #,(source-loc this-syntax)))
             #:with expansion  #`(parsed-signature-declaration (quote parsed)))
    (pattern ((~literal op) op-decl:str ...)
             #:attr parsed (parse-scribble-text (syntax/p operator/p) #'(op-decl ...))
             #:attr decl (list #`(cons (quote parsed) #,(source-loc this-syntax)))
             #:with expansion #`(parsed-signature-declaration (quote parsed)))

    ;; Rules
    (pattern ((~literal rule) label:identifier rule-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p rule/p) #'(rule-expr ...))
             #:attr decl (list #`(cons (list 'asset (quote label) (quote parsed)) #,(source-loc this-syntax)))
             #:with expansion #`(parsed-rule leibniz-doc current-context 
                                             (quote label) (quote parsed)
                                             #,(source-loc (first (syntax->list #'(rule-expr ...))))))
    (pattern ((~literal rule) label:identifier asset-ref:identifier
                              (~optional (~seq #:flip flip?:boolean) #:defaults ([flip? #'#f])))
             #:attr decl (list #`(cons (list 'asset (quote label)
                                             (list 'as-rule (quote asset-ref) flip?))
                                       #,(source-loc this-syntax)))
             #:with expansion #`(computed-rule leibniz-doc current-context
                                               (quote label) (quote asset-ref) flip?
                                               #,(source-loc #'label)))
    (pattern ((~literal rule) rule-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p rule/p) #'(rule-expr ...))
             #:attr decl (list #`(cons (quote parsed) #,(source-loc this-syntax)))
             #:with expansion #`(parsed-rule leibniz-doc current-context 
                                             #f (quote parsed)
                                             #,(source-loc (first (syntax->list #'(rule-expr ...))))))

    ;; Transformations
    (pattern ((~literal transformation) label:identifier tr-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p transformation/p) #'(tr-expr ...))
             #:attr decl (list #`(cons (list 'asset (quote label) (quote parsed)) #,(source-loc this-syntax)))
             #:with expansion #`(parsed-transformation leibniz-doc current-context 
                                                       (quote label) (quote parsed)
                                                       #,(source-loc (first (syntax->list #'(tr-expr ...))))))

    ;; Terms
    (pattern ((~literal term) label:identifier term-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p (to-eof/p term/p)) #'(term-expr ...))
             #:attr decl (list #`(cons (list 'asset (quote label) (quote parsed)) #,(source-loc this-syntax)))
             #:with expansion #`(parsed-term leibniz-doc current-context (quote label) (quote parsed) #,(source-loc (first (syntax->list #'(term-expr ...))))))
    (pattern ((~literal term) term-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p (to-eof/p term/p)) #'(term-expr ...))
             #:attr decl empty
             #:with expansion #`(parsed-term leibniz-doc current-context #f (quote parsed) #,(source-loc (first (syntax->list #'(term-expr ...))))))

    ;; Equations
    (pattern ((~literal equation) label:identifier equation-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p equation/p) #'(equation-expr ...))
             #:attr decl (list #`(cons (list 'asset (quote label) (quote parsed))
                                       #,(source-loc this-syntax)))
             #:with expansion #`(parsed-equation leibniz-doc current-context 
                                                 (quote label) (quote parsed)
                                                 #,(source-loc (first (syntax->list #'(equation-expr ...))))))
    (pattern ((~literal equation) label:identifier asset-ref:identifier)
             #:attr decl (list #`(cons (list 'asset (quote label)
                                             (list 'as-equation (quote asset-ref)))
                                       #,(source-loc this-syntax)))
             #:with expansion #`(computed-equation leibniz-doc current-context
                                                   (quote label) (quote asset-ref)
                                                   #,(source-loc #'label)))
    (pattern ((~literal equation) equation-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p equation/p) #'(equation-expr ...))
             #:attr decl empty
             #:with expansion #`(parsed-equation leibniz-doc current-context 
                                                 #f (quote parsed)
                                                 #,(source-loc (first (syntax->list #'(equation-expr ...))))))

    ;; Commented declarations - not stored in the context
    (pattern ((~literal comment-sort) sort-decl:str ...)
             #:attr parsed (parse-scribble-text (syntax/p sort-or-subsort/p) #'(sort-decl ...))
             #:attr decl empty
             #:with expansion  #`(parsed-signature-comment (quote parsed)))
    (pattern ((~literal comment-op) op-decl:str ...)
             #:attr parsed (parse-scribble-text (syntax/p operator/p) #'(op-decl ...))
             #:attr decl empty
             #:with expansion #`(parsed-signature-comment (quote parsed)))
    (pattern ((~literal test) rule-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p rule/p) #'(rule-expr ...))
             #:attr decl empty
             #:with expansion #`(parsed-test leibniz-doc current-context 
                                             (quote parsed)
                                             #,(source-loc (first (syntax->list #'(rule-expr ...))))))

    ;; Term evaluation
    (pattern ((~literal eval-term) term-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p (to-eof/p term/p))
                                                #'(term-expr ...))
             #:attr decl empty
             #:with expansion #`(parsed-eval-term leibniz-doc current-context
                                                  (quote parsed)
                                                  #,(source-loc
                                                     (first (syntax->list
                                                             #'(term-expr ...))))))

    ;; Substite or transform assets
    (pattern ((~literal substitute) label:identifier
                                    substitution-ref:identifier
                                    asset-ref:identifier
                                    (~optional (~seq #:reduce reduce?:boolean)
                                               #:defaults ([reduce? #'#f])))
             #:attr decl (list #`(cons (list 'asset (quote label)
                                             (list 'substitute
                                                   (quote substitution-ref)
                                                   (quote asset-ref)
                                                   reduce?))
                                       #,(source-loc this-syntax)))
             #:with expansion #`(asset-substitution leibniz-doc current-context
                                                    (quote label) (quote substitution-ref)
                                                    (quote asset-ref)
                                                    reduce?
                                                    #,(source-loc #'label)))

    (pattern ((~literal transform) label:identifier
                                   transformation-ref:identifier
                                   asset-ref:identifier
                                   (~optional (~seq #:reduce reduce?:boolean)
                                              #:defaults ([reduce? #'#f])))
             #:attr decl (list #`(cons (list 'asset (quote label)
                                             (list 'transform
                                                   (quote transformation-ref)
                                                   (quote asset-ref)
                                                   reduce?))
                                       #,(source-loc this-syntax)))
             #:with expansion #`(asset-transformation leibniz-doc current-context
                                                      (quote label) (quote transformation-ref)
                                                      (quote asset-ref)
                                                      reduce?
                                                      #,(source-loc #'label)))

    ;; References to assets
    (pattern ((~literal ref) label:identifier)
             #:attr decl empty
             #:with expansion #`(asset-reference leibniz-doc current-context
                                                 (quote label)))

    ;; Rendered contexts
    (pattern ((~literal show-context) name:str)
             #:attr decl empty
             #:with expansion #'(format-context
                                 (get-context leibniz-doc name)))
    (pattern ((~literal show-context))
             #:attr decl empty
             #:with expansion #'(format-context
                                 (get-context leibniz-doc current-context)))

    ;; Retrieve declarations in nested Scribble elements
    (pattern (element args:arg-or-kw-arg ...)
             #:attr decl (apply append (attribute args.decl))
             #:with expansion
             #`(element #,@(apply append
                                  (map syntax-e (syntax-e #'(args.arg-in-list ...))))))
    ;; Anything else: pass through
    (pattern any
             #:attr decl empty
             #:with expansion #'any))

  ;; Three modes for context inclusion: use, extend, insert with optional transformations
  (define-splicing-syntax-class context-ref
    (pattern (~seq #:use name:str)
             #:attr ref #`(cons '(use name) #,(source-loc #'name))
             #:attr verb #'"uses")
    (pattern (~seq #:extend name:str)
             #:attr ref #`(cons '(extend name) #,(source-loc #'name))
             #:attr verb #'"extends")
    (pattern (~seq #:insert-use [name:str tr:expr ...])
             #:attr ref #`(cons '(insert-use name tr ...) #,(source-loc #'name))
             #:attr verb #'"includes transformed")
    (pattern (~seq #:insert-extend [name:str tr:expr ...])
             #:attr ref #`(cons '(insert-extend name tr ...) #,(source-loc #'name))
             #:attr verb #'"includes transformed")))

(define-syntax (context stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ name:str
          include:context-ref ...
          body:body-item ...)
       #`(begin (set! #,leibniz-ref
                      (add-context-from-source
                       #,leibniz-ref name
                       (append
                        (list include.ref ...)
                        #,(cons #'list (apply append (attribute body.decl))))))
                (margin-note "Context " (italic name)
                             (list (linebreak)
                                   include.verb " " (italic include.name)) ...)
                (let ([leibniz-doc #,leibniz-ref]
                      [current-context name])
                  (list body.expansion ...)))])))

;;
;; Import library modules
;;
(define-syntax (import stx)
  (syntax-parse stx
    ;; XML document by filename
    [(_ name:str source:str)
     (with-syntax ([leibniz-id (datum->syntax #'name 'leibniz)])
       #`(set! leibniz-id (import-xml leibniz-id name source)))]
    ;; Racket module by module name (likely to be removed in the future)
    [(_ name:str source:expr)
     (with-syntax ([library-id (format-id #'name "library/~a" (syntax-e #'name))]
                   [leibniz-id (datum->syntax #'name 'leibniz)])
       #'(begin (require (only-in source [leibniz-id library-id]))
                (set! leibniz-id
                      (add-to-library leibniz-id name library-id))))]))

;;
;; Raise errors when Leibniz code is used outside of a context
;;
(define-syntax (context-syntax-error-handler stx)
  (syntax-parse stx
    [(_ name arg ...)
     #'(define-syntax (name stxx)
         (raise-syntax-error #f
                             "illegal syntax or used outside of a context definition"
                             stxx))]))

(context-syntax-error-handler sort)
(context-syntax-error-handler var)
(context-syntax-error-handler op)
(context-syntax-error-handler term)
(context-syntax-error-handler rule)
(context-syntax-error-handler equation)
(context-syntax-error-handler transformation)
(context-syntax-error-handler comment-sort)
(context-syntax-error-handler comment-op)
(context-syntax-error-handler test)
(context-syntax-error-handler eval-term)
(context-syntax-error-handler ref)
(context-syntax-error-handler substitute)
(context-syntax-error-handler transform)

;;
;; show-context is the only Leibniz element that can be used outside of
;; a context block.
;;
(define-syntax (show-context stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ name:str)
       #`(format-context (get-context #,leibniz-ref name))])))

;;
;; Generate the Scribble representation of the various Leibniz context elements
;;
(define (parsed-term leibniz-doc current-context label parsed-term-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define term (make-term leibniz-doc current-context parsed-term-expr (list loc)))
  (define sort-str (sorts:constraint->string (operators:signature-sort-graph signature)
                                            (terms:term.sort term)))
  (define term-elem (format-term signature label term))
  (leibniz-input-with-hover sort-str term-elem))

(define (parsed-rule leibniz-doc current-context label parsed-rule-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define rule (make-rule leibniz-doc current-context parsed-rule-expr (list loc)))
  (define sorts-str (equations:rule-sorts-str signature rule))
  (leibniz-input-with-hover sorts-str
                            (format-rule label rule signature)))

(define (parsed-transformation leibniz-doc current-context label parsed-tr-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define tr (make-transformation leibniz-doc current-context label parsed-tr-expr (list loc)))
  (define sorts-str (equations:transformation-sorts-str signature tr))
  (leibniz-input-with-hover sorts-str
                            (format-transformation label tr signature)))

(define (parsed-equation leibniz-doc current-context label parsed-equation-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define equation (make-equation leibniz-doc current-context parsed-equation-expr (list loc)))
  (define sorts-str (equations:equation-sorts-str signature equation))
  (leibniz-input-with-hover sorts-str
                            (format-equation label equation signature)))

(define (parsed-test leibniz-doc current-context parsed-rule-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define test (make-test leibniz-doc current-context parsed-rule-expr (list loc)))
  (define success (equal? (second test) (third test)))
  (list
   (leibniz-input
    (format-term signature #f (first test))
    " ⇒ "
    (format-term signature #f (second test)))
   (nonbreaking (hspace 1))
   (leibniz-output
    (if success
        (elem #:style (style #f (list (color-property "green")))
              "✓")
        (list "❌ " (format-term signature #f (third test)))))))

(define (parsed-eval-term leibniz-doc current-context parsed-term-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define rules (hash-ref context 'compiled-rules))
  (define term (make-term leibniz-doc current-context parsed-term-expr (list loc)))
  (define sort-str(sorts:constraint->string (operators:signature-sort-graph signature)
                                            (terms:term.sort term)))
  (define term-elem (format-term signature #f term))
  (define rterm (rewrite:reduce signature rules term))
  (define rsort-str(sorts:constraint->string (operators:signature-sort-graph signature)
                                             (terms:term.sort rterm)))
  (define rterm-elem (format-term signature #f rterm))
  (list
   (leibniz-input-with-hover sort-str term-elem)
   (leibniz-output-with-hover rsort-str " ⇒ " rterm-elem)))

(define (parsed-signature-declaration decl)
  (nonbreaking (leibniz-input (format-signature-declaration decl))))

(define (parsed-aignature-comment decl)
  (nonbreaking (leibniz-comment (format-signature-declaration decl))))

(define (asset-reference leibniz-doc current-context label)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define asset (get-asset context label))
  (leibniz-input-with-hover (plain-text (format-asset label asset signature))
                            (format-asset-reference label asset)))

(define (computed-equation leibniz-doc current-context label asset-ref loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define asset (get-asset context asset-ref))
  (define equation (get-asset context label))
  (define sorts-str (equations:equation-sorts-str signature equation))
  (list
   (leibniz-input (format-label label))
   (leibniz-input-with-hover (plain-text (format-asset asset-ref asset signature))
                             (list "=(" (format-label asset-ref #f) ") "))
   (leibniz-output-with-hover sorts-str
                              (format-equation #f equation signature))))

(define (computed-rule leibniz-doc current-context label asset-ref flip? loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define asset (get-asset context asset-ref))
  (define rule (get-asset context label))
  (define sorts-str (equations:rule-sorts-str signature rule))
  (list
   (leibniz-input (format-label label))
   (leibniz-input-with-hover (plain-text (format-asset asset-ref asset signature))
                             (list (if flip? "⇐(" "⇒(") (format-label asset-ref #f) ") "))
   (leibniz-output-with-hover sorts-str
                              (format-rule #f rule signature))))

(define (asset-sort-str signature asset)
  (cond
    [(equations:equation? asset)
     (equations:equation-sorts-str signature asset)]
    [(equations:rule? asset)
     (equations:rule-sorts-str signature asset)]
    [(equations:transformation? asset)
     (equations:transformation-sorts-str signature asset)]
    [(terms:term? asset)
     (sorts:constraint->string (operators:signature-sort-graph signature)
                               (terms:term.sort asset))]
    [else
     (error (format "unknown asset type: ~a" asset))]))

(define (asset-substitution leibniz-doc current-context label rule-ref asset-ref reduce? loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define substituted-asset (get-asset context label))
  (define rule (get-asset context rule-ref))
  (define asset (get-asset context asset-ref))
  (list
   (leibniz-input (format-label label))
   (leibniz-input-with-hover (plain-text (format-asset rule-ref rule signature))
                             (format-label rule-ref #f))
   (leibniz-input "(")
   (leibniz-input-with-hover (plain-text (format-asset asset-ref asset signature))
                             (format-label asset-ref #f))
   (if reduce?
       (leibniz-input ") ⇩ ")
       (leibniz-input ") "))
   (leibniz-output-with-hover (asset-sort-str signature substituted-asset)
                              (format-asset #f substituted-asset signature))))

(define (asset-transformation leibniz-doc current-context label tr-ref asset-ref reduce? loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define transformed-asset (get-asset context label))
  (define tr (get-asset context tr-ref))
  (define asset (get-asset context asset-ref))
  (list
   (leibniz-input (format-label label))
   (leibniz-input-with-hover (plain-text (format-asset asset-ref asset signature))
                             (format-label asset-ref #f))
   (leibniz-input " ⁞ ")
   (leibniz-input-with-hover (plain-text (format-asset tr-ref tr signature))
                             (format-label tr-ref #f))
   (if reduce?
       (leibniz-input " ⇩ ")
       (leibniz-input " "))
   (leibniz-output-with-hover (asset-sort-str signature transformed-asset)
                              (format-asset #f transformed-asset signature))))

;;
;; Tests
;;
(module+ test

  ;; A context in #lang leibniz syntax (converted to s-exp)
  (define leibniz empty-document)
  (define scribble-elements
    (context
     "test-context"
     #:use "builtins/truth"
     (sort "A")
     (sort "Qnz ⊆ Q")
     (var "an-A:A")
     (op "A + A : A")
     (op "foo(A, A) : A")
     (op "bar(x:A, A) : A")
     (rule "a:A ⇒ foo(a, a)")
     (rule a-rule "a:A ⇒ bar(a, a)")
     (term a-term "bar(x, x)")
     (term a.nested.asset "bar(x, x)")
     (equation an-equation "bar(x, x) = foo(a, a) ∀ x:A ∀ a:A")))

  ;; The same context in the input syntax of documents.rkt
  (define declarations
    (list (cons '(use "builtins/truth") #f)
          (cons '(sort A) #f)
          (cons '(subsort Qnz Q) #f)
          (cons '(var an-A A) #f)
          (cons '(op _+ ((sort A) (sort A)) A) #f)
          (cons '(op foo ((sort A) (sort A)) A) #f)
          (cons '(op bar ((var x A) (sort A)) A) #f)
          (cons '(rule (term-or-var a)
                       (term foo ((term-or-var a) (term-or-var a)))
                       ((var a A))) #f)
          (cons '(asset a-rule
                        (rule (term-or-var a)
                              (term bar ((term-or-var a) (term-or-var a)))
                              ((var a A)))) #f)
          (cons '(asset a-term (term bar ((term-or-var x) (term-or-var x)))) #f)
          (cons '(asset a.nested.asset (term bar ((term-or-var x) (term-or-var x)))) #f)
          (cons '(asset an-equation
                        (equation (term bar ((term-or-var x) (term-or-var x)))
                                  (term foo ((term-or-var a) (term-or-var a)))
                                  ((var x A) (var a A)))) #f)))

  ;; Check equivalence of the two context specifications
  ;; We need to remove the compiled signatures and the locs
  ;; because context1 has location information whereas context2
  ;; does not.
  (define context1 (~> (get-context leibniz "test-context")
                       (clean-declarations)
                       (hash-remove 'locs)))
  (define context2 (~> empty-document
                    (add-context-from-source "test-context" declarations)
                    (get-context "test-context")
                    (clean-declarations)
                    (hash-remove 'locs)))
  (check-equal? context1 context2))
