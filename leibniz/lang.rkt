#lang racket

(provide (all-from-out scribble/doclang
                       scribble/base)
         empty-document
         import
         context insert-context show-context
         sort var op term rule equation
         comment-sort comment-op
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
         (prefix-in sorts: "./sorts.rkt")
         (prefix-in operators: "./operators.rkt")
         (prefix-in terms: "./terms.rkt")
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
             #:attr decl empty
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
    (pattern (~seq #:use name:str)
             #:attr ref #`(cons name #,(source-loc #'name)))))

(define-syntax (context stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ name:str
          include:context-ref ...
          (~seq #:from-context context:expr)
          body:body-item ...)
       #`(begin (set! #,leibniz-ref (add-context #,leibniz-ref name
                                                 (list include.ref ...)
                                                 context))
                (unless (empty? #,(cons 'list (apply append (attribute body.decl))))
                  (error "Inserted context may not be extended"))
                (margin-note "Context " (italic name)
                             (list (linebreak)
                                   "uses " (italic include.name)) ...)
                (let ([leibniz-doc #,leibniz-ref]
                      [current-context name])
                  (list body.expansion ...)))]
      [(_ name:str
          include:context-ref ...
          body:body-item ...)
       #`(begin (set! #,leibniz-ref (new-context #,leibniz-ref name
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
   #".Leibniz { background-color: #E8E8FF; }\n.LeibnizOutput { background-color: #E0FFE0; }\n.LeibnizComment { background-color: #FFE8E8; }\n"))

(define leibniz-style (style "Leibniz" (list leibniz-css)))
(define leibniz-output-style (style "LeibnizOutput" (list leibniz-css)))
(define leibniz-comment-style (style "LeibnizComment" (list leibniz-css)))

(define (format-sort symbol)
  (if symbol
      (italic (symbol->string symbol))
      (italic "<any>")))

(define (format-var name sort)
  (list (italic (symbol->string name))
        ":"
        (format-sort sort)))

(define (format-var-or-sort var-or-sort-decl)
  (match var-or-sort-decl
    [(list 'sort sort) (format-sort sort)]
    [(list 'var name sort) (format-var name sort)]))

(define (format-subsort-declaration sort1 sort2)
  (list (format-sort sort1)
        " ⊆ "   ; MEDIUM MATHEMATICAL SPACE ; SUBSET OF OR EQUAL TO
        (format-sort sort2)))

(define (op-symbol-and-type op-symbol)
  (define s (symbol->string op-symbol))
  (cond
    [(member s '("[]" "^" "_")) (values s 'special-op)]
    [(string-prefix? s "_") (values (substring s 1) 'infix-op)]
    [else (values s 'prefix-op)]))

(define (op-type op-symbol)
  (define-values (op-str type) (op-symbol-and-type op-symbol))
  type)

(define (format-op-declaration op-symbol arg-decls result-sort)
  (define-values (op-str op-type) (op-symbol-and-type op-symbol))
  (case op-type
    [(prefix-op)
     (list op-str
           (if (zero? (length arg-decls))
               ""
               (list "(" (add-between (map format-var-or-sort arg-decls) ", ") ")"))
           " : "
           (format-sort result-sort))]
    [(infix-op)
     (list (format-var-or-sort (first arg-decls))
           " " op-str " "
           (format-var-or-sort (second arg-decls))
           " : "
           (format-sort result-sort))]
    [(special-op)
     (case op-str
       [("[]")
        (list (format-var-or-sort (first arg-decls))
              "["
              (add-between (map format-var-or-sort (rest arg-decls)) ", ")
              "] : "
              (format-sort result-sort))]
       [("_")
        (list (format-var-or-sort (first arg-decls))
              (subscript (format-var-or-sort (second arg-decls)))
              " : "
              (format-sort result-sort))]
       [("^")
        (list (format-var-or-sort (first arg-decls))
              (superscript (format-var-or-sort (second arg-decls)))
              " : "
              (format-sort result-sort))])]))

(define (format-declaration decl)
  (match decl
    [(list 'sort sort-symbol)
     (format-sort sort-symbol)]
    [(list 'subsort sort-symbol-1 sort-symbol-2)
     (format-subsort-declaration sort-symbol-1 sort-symbol-2)]
    [(list 'op op-symbol arg-sorts result-sort)
     (format-op-declaration op-symbol arg-sorts result-sort)]
    [(list 'var name sort)
     (format-var name sort)]))

(define (parsed-declaration decl)
  (nonbreaking (elem #:style leibniz-style (format-declaration decl))))

(define (parsed-comment decl)
  (nonbreaking (elem #:style leibniz-comment-style (format-declaration decl))))

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

(define (format-rule rule context)
  (define signature (contexts:context-signature context))
  (define c-vars (terms:all-vars (contexts:context-vars context)))
  (define proc-rule? (procedure? (equations:rule-replacement rule)))
  (define pattern-elem (format-term signature (equations:rule-pattern rule)))
  (define replacement-elem
    (if proc-rule? 
        (italic "<procedure>")
        (format-term signature (equations:rule-replacement rule))))
  (define vars (terms:term.vars (equations:rule-pattern rule)))
  (define cond (equations:rule-condition rule))
  (define var-elems (for/list ([var (in-set vars)]
                               #:unless (equal? (hash-ref c-vars
                                                          (terms:var-name var)
                                                          #f)
                                                (terms:var-sort var)))
                      (list (linebreak) (hspace 2)
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
  (list pattern-elem
        (if proc-rule? " → "  " ⇒ ")
        replacement-elem
        var-elems
        cond-elem))

(define (format-equation equation context)
  (define signature (contexts:context-signature context))
  (define vars (set-union (terms:term.vars (equations:equation-left equation))
                          (terms:term.vars (equations:equation-right equation))))
  (define cond (equations:equation-condition equation))
  (define left-elem (format-term signature (equations:equation-left equation)))
  (define right-elem (format-term signature (equations:equation-right equation)))
  (define var-elems (for/list ([var (in-set vars)])
                      (list (linebreak ) (hspace 2)
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
  (list left-elem
        " = "
        right-elem
        var-elems
        cond-elem))

(define (parsed-term leibniz-doc current-context parsed-term-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define signature (contexts:context-signature context))
  (define vars (contexts:context-vars context))
  (define term (make-term leibniz-doc current-context parsed-term-expr loc))
  (define sort-str(sorts:constraint->string (operators:signature-sort-graph signature)
                                            (terms:term.sort term)))
  (define term-elem (format-term signature term))
  (elem #:style (style "Leibniz" (list leibniz-css (hover-property sort-str)))
        term-elem))

(define (parsed-rule leibniz-doc current-context parsed-rule-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define rule (make-rule leibniz-doc current-context parsed-rule-expr loc))
  (elem #:style leibniz-style
        (format-rule rule context)))

(define (parsed-equation leibniz-doc current-context parsed-equation-expr loc)
  (define context (get-context leibniz-doc current-context))
  (define equation (make-equation leibniz-doc current-context parsed-equation-expr loc))
  (elem #:style leibniz-style
        (format-equation equation context)))

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

; Insert and format a context referred to by name

; This syntax transformer is only used outside of a context block.
(define-syntax (show-context stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ name:str)
       #`(format-context-declarations (get-context-declarations #,leibniz-ref name))])))

(define (format-context-declarations decls)
  (list (format-sort-declarations (hash-ref decls 'sorts))
        (format-op-declarations (hash-ref decls 'ops))
        (format-var-declarations (hash-ref decls 'vars))
        (format-rule-declarations (hash-ref decls 'rules) (hash-ref decls 'vars))))

(define (format-sort-declarations sort-decls)
  (define sorts
    (for/list ([sd sort-decls]
               #:when (equal? (first sd) 'sort))
      (elem #:style leibniz-output-style
            (format-sort (second sd)))))
  (define subsorts
    (for/list ([sd sort-decls]
               #:when (equal? (first sd) 'sort))
      (elem #:style leibniz-output-style
            (format-subsort-declaration (second sd) (third sd)))))
  (if (empty? sort-decls)
      ""
      (list (if (empty? sorts)
                ""
                (list "Sorts: " (add-between sorts ", ") (linebreak)))
            (if (empty? sorts)
                ""
                (list "Subsort relations: " (add-between subsorts ", ") (linebreak)))
            (linebreak))))

(define (format-op-declarations op-decls)
  (define ops
    (for/list ([od op-decls])
      (elem #:style leibniz-output-style
            (apply format-op-declaration (rest od)))))
  (if (empty? op-decls)
      ""
      (list "Operators:" (linebreak)
            (apply nested
                   (add-between ops (linebreak))
                   #:style 'inset))))

(define (format-var-declarations var-decls)
  (define vars
    (for/list ([vd var-decls])
      (elem #:style leibniz-output-style
            (format-var (second vd) (third vd)))))
  (if (empty? var-decls)
      ""
      (list "Vars: "
            (add-between vars ", ")
            (linebreak)
            (linebreak))))

(define (format-rule-declarations rule-decls var-decls)
  (define context-vars
    (for/hash ([vd var-decls])
      (values (second vd) (third vd))))
  (define rules
    (for/list ([rd rule-decls])
      (elem #:style leibniz-output-style
            (format-rule-declaration rd context-vars))))
  (if (empty? rule-decls)
      ""
      (list "Rules:" (linebreak)
            (apply nested
                   (add-between rules (linebreak))
                   #:style 'inset))))

(define (format-rule-declaration decl context-vars)
  (define pattern-elem (format-decl-term (second decl)))
  (define proc-rule? (procedure? (second decl)))
  (define replacement-elem
    (if proc-rule?
        (italic "<procedure>")
        (format-decl-term (third decl))))
  (define clause-elems
    (for/list ([clause (fourth decl)])
      (if (equal? (first clause) 'var)
          (let ([name (second clause)]
                [sort (third clause)])
            (if (equal? (hash-ref context-vars name #f) sort)
                ""
                (list (linebreak) (hspace 2)
                      "∀ "
                      (format-var name sort))))
          (list (linebreak) (hspace 2)
                "if "
                (format-decl-term clause)))))
  (list* pattern-elem
         (if proc-rule? " → "  " ⇒ ")
         replacement-elem
         clause-elems))

(define (format-decl-term decl-term)

  (define (infix-decl-term? decl-term)
    (and (equal? (first decl-term) 'term)
         (equal? (op-type (second decl-term)) 'infix-op)))

  (match decl-term
    [(list 'term raw-op args)
     (define-values (op type) (op-symbol-and-type raw-op))
     (condd
      [(zero? (length args))               ; no args implies prefix-op
       op]
      [(equal? type 'prefix-op)
       (list op
             "("
             (add-between (for/list ([arg args]) (format-decl-term arg)) ", ")
             ")")]
      #:do (define arg1 (first args))
      #:do (define f-arg1 (format-decl-term arg1))
      [(equal? op "[]")
       (list (parenthesize f-arg1 (infix-decl-term? arg1))
             "["
             (add-between (for/list ([arg (rest args)]) (format-decl-term arg)) ", ")
             "]")]
      #:do (define arg2 (second args))
      #:do (define f-arg2 (format-decl-term arg2))
      [(equal? type 'infix-op)
       (list (parenthesize f-arg1 (infix-decl-term? arg1))
             " " op  " " f-arg2)]
      [(equal? op "^")
       (list f-arg1 (superscript f-arg2))]
      [(equal? op "_")
       (list f-arg1 (subscript f-arg2))])]
    [(list (or 'integer 'rational 'floating-point) n)
     (term->string n)]
    [_ (error (format "illegal term ~a" decl-term))]))

; Insert and format a context given as a data structure

(define-syntax (insert-context stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ name:str context:expr)
       #`(begin (set! #,leibniz-ref (add-context #,leibniz-ref name empty context))
                (margin-note "Context " (italic name))
                (format-context context))])))

(define (format-context context)
  (list
   (format-signature (contexts:context-signature context))
   "Rules:" (linebreak)
   (for/list ([rule (equations:in-rules (contexts:context-rules context))])
     (list (elem #:style leibniz-output-style
                 (format-rule rule context))
           (linebreak)))
   "Equations:" (linebreak)
   (for ([eq (equations:in-equations (contexts:context-equations context))])
     (list (elem #:style leibniz-output-style
                 (format-equation eq context))
           (linebreak)))))

(define (format-signature signature)
  (list
   (format-sort-graph (operators:signature-sort-graph signature))
   "Operators:" (linebreak)
   (for/list ([(symbol rank meta) (operators:all-ops signature)])
     (list (elem #:style leibniz-output-style
                 (format-op-declaration symbol (car rank) (cdr rank)))
           (linebreak)))))

(define (format-sort-graph sort-graph)
  (define ccs (sorts:connected-components sort-graph))
  (for/list ([cc ccs])

    (define sorts (sorts:all-sorts cc))
    (define subsorts (sorts:all-subsort-relations cc))

    (list

     (if (equal? (length ccs) 1)
         ""
         (list "Kind " (sorts:constraint->string cc sorts) (linebreak)))

     (if (equal? (set-count sorts) 1) "Sort: " "Sorts: ")
     (add-between
      (for/list ([sort (in-set sorts)])
        (elem #:style leibniz-output-style (format-sort sort)))
      ", ")
     (linebreak)

     (if (zero? (set-count subsorts))
         ""
         (list
          "Subsort relations: "
          (add-between
           (for/list ([ss (in-set subsorts)])
             (elem #:style leibniz-output-style
                   (format-subsort-declaration (car ss) (cdr ss))))
           ", ")
          (linebreak))))))

; Support code for nicer formatting

(define (inset . body)
  (apply nested
         (for/list ([element body])
           (if (equal? element "\n")
               (linebreak)
               element))
         #:style 'inset))
