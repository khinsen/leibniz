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
         xml signature-graphs
         use show reduce trace transform substitute)

(require scribble/doclang
         scribble/base
         scribble/core
         scribble/html-properties
         (for-syntax syntax/parse
                     racket/syntax
                     racket/list
                     "./parser.rkt"
                     megaparsack (except-in megaparsack/text integer/p)
                     data/monad
                     data/applicative)
         "./documents.rkt"
         "./transformations.rkt"
         "./parser.rkt"
         "./formatting.rkt"
         (only-in megaparsack/text parse-string)
         (only-in megaparsack parse-result! parse-error->string)
         data/either
         threading
         (prefix-in sorts: "./sorts.rkt")
         (prefix-in operators: "./operators.rkt")
         (prefix-in terms: "./terms.rkt")
         (prefix-in equations: "./equations.rkt")
         (prefix-in rewrite: "./rewrite.rkt"))

;; Translate the location information from a syntax object into strings and numbers
;; that can be used at runtime.

(begin-for-syntax
  (define (source-loc stx)
    (list 'list
          (syntax-source stx)
          (syntax-line stx)
          (+ 1 (syntax-column stx))
          (syntax-position stx)
          (syntax-span stx))))

;;
; Context definitions
;;
;; All Leibniz declarations are collected from the body of a context and used to define
;; the context inside the single "document" object referred to by "leibniz".

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
             #:with expansion  #`(parsed-declaration (quote parsed)))
    (pattern ((~literal var) var-decl:str ...)
             #:attr parsed (parse-scribble-text (syntax/p (to-eof/p var/p)) #'(var-decl ...))
             #:attr decl (list #`(cons (quote parsed) #,(source-loc this-syntax)))
             #:with expansion  #`(parsed-declaration (quote parsed)))
    (pattern ((~literal op) op-decl:str ...)
             #:attr parsed (parse-scribble-text (syntax/p operator/p) #'(op-decl ...))
             #:attr decl (list #`(cons (quote parsed) #,(source-loc this-syntax)))
             #:with expansion #`(parsed-declaration (quote parsed)))
    ;; Rules
    (pattern ((~literal rule) rule-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p rule/p) #'(rule-expr ...))
             #:attr decl (list #`(cons (quote parsed) #,(source-loc this-syntax)))
             #:with expansion #`(parsed-rule leibniz-doc current-context 
                                             (quote parsed)
                                             #,(source-loc (first (syntax->list #'(rule-expr ...))))))

    ;; Terms
    (pattern ((~literal term) term-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p (to-eof/p term/p)) #'(term-expr ...))
             #:attr decl empty
             #:with expansion #`(parsed-term leibniz-doc current-context (quote parsed) #,(source-loc (first (syntax->list #'(term-expr ...))))))
    ;; Equations
    (pattern ((~literal equation) equation-expr:str ...)
             #:attr parsed (parse-scribble-text (syntax/p equation/p) #'(equation-expr ...))
             #:attr as-asset (syntax-case #'parsed () [(_ label left right clauses) #'(asset label (equation left right clauses))])
             #:attr decl (list #`(cons (quote as-asset) #,(source-loc this-syntax)))
             #:with expansion #`(parsed-equation leibniz-doc current-context 
                                                 (quote parsed)
                                                 #,(source-loc (first (syntax->list #'(equation-expr ...))))))
    ;; Commented declarations - not stored in the context
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
    (pattern ((~literal show-context) name:str)
             #:attr decl empty
             #:with expansion #'(format-context-declarations
                                 (get-context leibniz-doc name)))
    (pattern ((~literal show-context))
             #:attr decl empty
             #:with expansion #'(format-context-declarations
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
    (pattern (~seq #:insert [name:str tr:expr ...])
             #:attr ref #`(cons '(insert name tr ...) #,(source-loc #'name))
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
;; Generate the Scribble representation of the various Leibniz context elements
;;
(define (parsed-term leibniz-doc current-context parsed-term-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define term (make-term leibniz-doc current-context parsed-term-expr loc))
  (define sort-str(sorts:constraint->string (operators:signature-sort-graph signature)
                                            (terms:term.sort term)))
  (define term-elem (format-term signature term))
  (leibniz-input-with-hover sort-str term-elem))

(define (parsed-rule leibniz-doc current-context parsed-rule-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define rule (make-rule leibniz-doc current-context parsed-rule-expr loc))
  (leibniz-input (format-rule rule signature)))

(define (parsed-equation leibniz-doc current-context parsed-equation-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
  (define label (second parsed-equation-expr))
  (define equation (make-equation leibniz-doc current-context (cons 'equation (rest (rest parsed-equation-expr))) loc))
  (leibniz-input (format-equation label equation signature)))

(define (parsed-test leibniz-doc current-context parsed-rule-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (hash-ref context 'compiled-signature))
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
  (define signature (hash-ref context 'compiled-signature))
  (define rules (hash-ref context 'compiled-rules))
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

;;
;; Raise errors when Leibniz code is used outside of a context
;;
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

;;
;; show-context is the only Leibniz element that can be used outside of
;; a context block.
;;
(define-syntax (show-context stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ name:str)
       #`(format-context-declarations (get-context #,leibniz-ref name))])))

;;
;; Support code for nicer formatting
;;
(define (inset . body)
  (apply nested
         (for/list ([element body])
           (if (equal? element "\n")
               (linebreak)
               element))
         #:style 'inset))

;;
;;; Generate XML output (not needed with the leibniz script)
;;
(define-syntax (xml stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ filename:str)
       #`(begin (write-xml #,leibniz-ref filename)
                (margin-note (hyperlink filename "XML")))])))

;;
;; Graphviz output (for debugging)
;;
(define-syntax (signature-graphs stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ directory:str)
       #`(write-signature-graphs #,leibniz-ref directory)])))

;;
;; Support for interactive exploration
;; REPL commands for use after loading a Leibniz module
;;
(define current-context-name (make-parameter #f))
(define current-context (make-parameter #f))
(define current-document (make-parameter #f))

(define-syntax (use stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ context-name:str)
       #`(begin
           (define context
             (with-handlers ([exn:fail? (λ (e) #f)])
               (get-context #,leibniz-ref context-name)))
           (unless context
             (error (format "Undefined context ~a" context-name)))
           (current-document #,leibniz-ref)
           (current-context-name context-name)
           (current-context context))])))

(define (assert-current-context)
  (unless (current-context-name)
    (error "No context has been selected")))

(define (parse-term term-str)
  (parse-string (to-eof/p term/p) term-str))

(define (parse-equation eq-str)
  (parse-string equation/p eq-str))

(define (parse-transformation tr-str)
  (parse-string transformation/p tr-str))

(define (make-parsed-term term-str)
  (match (parse-term term-str)
    [(success parsed-term)
     (make-term (current-document) (current-context-name) parsed-term #f)]
    [(failure message)
     #f]))

(define (make-parsed-term-or-eq str)
  (match (parse-equation str)
    [(success parsed-eq)
     (displayln parsed-eq)
     (make-equation (current-document) (current-context-name) parsed-eq #f)]
    [(failure message)
     (make-parsed-term str)]))

(define (make-parsed-transformation transformation-str)
  (make-transformation (current-document) (current-context-name)
                       (~> transformation-str
                           parse-transformation
                           parse-result!)
                       #f))

(define (display-term signature term)
  (when term
    (displayln (plain-text (format-term signature term)))))

(define (display-equation signature equation)
  (when equation
    (displayln (plain-text (format-equation #f equation signature)))))

(define (show term-str)
  (assert-current-context)
  (define signature (hash-ref (current-context) 'compiled-signature))
  (define term (make-parsed-term term-str))
  (display-term signature term))

(define (reduce term-str)
  (assert-current-context)
  (define signature (hash-ref (current-context) 'compiled-signature))
  (define rules (hash-ref (current-context) 'compiled-rules))
  (define term (make-parsed-term term-str))
  (define rterm (and term
                     (rewrite:reduce signature rules term)))
  (display-term signature rterm))

(define (trace term-str
               #:max-level [max-level 0]
               #:show-rules [show-rules #f])
  (assert-current-context)
  (define (display-trace level term rule rterm)
    (cond
      [(equal? level 0)
       (when show-rules
         (displayln (format "--- ~a"
                            (plain-text (format-rule rule signature)))))
       (displayln (format "... ~a"
                         (plain-text (format-term signature rterm))))]
      [(or (not max-level)
           (<= level max-level))
       (when show-rules
         (displayln (format "~a ~a"
                            (make-string (+ level 3) #\-)
                            (plain-text (format-rule rule signature)))))
       (displayln (format "~a ~a ⇒ ~a"
                          (make-string (+ level 3) #\+)
                          (plain-text (format-term signature term))
                          (plain-text (format-term signature rterm))))]))
  (define signature (hash-ref (current-context) 'compiled-signature))
  (define rules (hash-ref (current-context) 'compiled-rules))
  (define term (make-parsed-term term-str))
  (when term
    (rewrite:trace-reduce signature rules term display-trace))
  (void))

(define (transform term-or-eq-str transformation-str)
  (define signature (hash-ref (current-context) 'compiled-signature))
  (define sort-graph (operators:signature-sort-graph signature))
  (define term-or-eq (make-parsed-term-or-eq term-or-eq-str))
  (define transformation (make-parsed-transformation transformation-str))
  (cond
    [(terms:term? term-or-eq)
     (display-term signature
      (rewrite:transform signature transformation term-or-eq))]
    [(equations:equation? term-or-eq)
     (display-equation signature
      (rewrite:transform-equation signature transformation term-or-eq))]))

(define (substitute term-or-eq-str transformation-str)
  (define signature (hash-ref (current-context) 'compiled-signature))
  (define sort-graph (operators:signature-sort-graph signature))
  (define term-or-eq (make-parsed-term-or-eq term-or-eq-str))
  (define transformation (make-parsed-transformation transformation-str))
  (cond
    [(terms:term? term-or-eq)
     (display-term signature
      (rewrite:substitute signature transformation term-or-eq))]
    [(equations:equation? term-or-eq)
     (display-equation signature
      (rewrite:substitute-equation signature transformation term-or-eq))]))
