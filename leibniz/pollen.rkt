#lang racket

;; Pollen interface 

(provide root
         +import
         +context +use +extend +context-ref
         +sort
         +op
         +var
         +term +eval-term +trace +substitute-context
         +rule
         +equation
         +test
         section subsection subsubsection subsubsubsection)

(require (submod txexpr safe)
         pollen/core
         pollen/decode
         pollen/tag
         pollen/unstable/typography
         megaparsack megaparsack/parser-tools/lex
         data/either
         "./condd.rkt"
         "./parser.rkt"
         "./formatting.rkt"
         (prefix-in contexts: "./contexts.rkt")
         (prefix-in documents: "./documents.rkt")
         (only-in "./builtin-contexts.rkt" current-context-name-resolver)
         (for-syntax syntax/parse)
         xml
         threading)

(module+ test
  (require rackunit))

;; Parsing

(begin-for-syntax
  ;; still used in use/extend
  (define (source-loc stx)
    (list 'list
          (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx))))

(define (leibniz-syntax-error tag text position unexpected expected)
  (define errno (error-counter))
  (error-counter (+ 1 errno))
  (syntax-error->html errno tag text position unexpected expected))

(define (with-parsed-text tag label parser text-parts fn)
  (define text (string-normalize-spaces (apply string-append text-parts)))
  (define tokens (lex-leibniz text))
  (define result (parse-tokens (to-eof/p parser) tokens))
  (if (failure? result)
      (match (from-failure #f result)
        [(message loc unexpected expected)
         (match loc
           [(srcloc source line column position span)
            (leibniz-syntax-error tag text column unexpected expected)])])
      (fn (from-success #f result) text label)))

(define-syntax (define-leibniz-parser stx)
  (syntax-parse stx
    [(_ tag:id parser:expr decl text body-exprs ...)
     #'(define (tag . text-parts)
         (with-parsed-text (quote tag) #f parser text-parts
           (λ (decl text label)
             body-exprs ...)))]))

(define-syntax (define-leibniz-parser-with-label stx)
  (syntax-parse stx
    [(_ tag:id parser:expr decl-name text-name label-name body-exprs ...)
     #'(define (tag #:label label . text-parts)
         (with-parsed-text (quote tag) label parser text-parts
           (λ (decl-name text-name label-name)
             body-exprs ...)))]))

(define-syntax (define-leibniz-parser-with-optional-label stx)
  (syntax-parse stx
    [(_ tag:id parser:expr decl-name text-name label-name body-exprs ...)
     #'(define (tag #:label [label #f] . text-parts)
         (with-parsed-text (quote tag) label parser text-parts
           (λ (decl-name text-name label-name)
             body-exprs ...)))]))

;;
;; Produce an error message in HTML. Show an error message and the
;; element that caused it.
;;
(define error-counter (make-parameter 1))

(define (leibniz-error message element)
  (define errno (error-counter))
  (error-counter (+ 1 errno))
  (error->html errno message element)
  `(@ (span ((class "LeibnizErrorMessage")
             (id ,(format "error~a" errno)))
            ,(format "Error #~a:" errno))
      (span ((class "LeibnizError"))
            ,(format "~a" element))
      (span ((class "LeibnizErrorMessage"))
            ,message)))

;;
;; Extract specific elements from an element list
;;
(define ((has-tag? tag) element)
  (and (txexpr? element)
       (equal? (get-tag element) tag)))

(define (extract-elements tag elements)
  (define-values (filtered-txexpr extracted-elements)
    (splitf-txexpr (txexpr 'dummy empty elements)
                   (has-tag? tag)))
  (values (get-elements filtered-txexpr) extracted-elements))

;;
;; The root function, which does most of the processing work
;; First, the document is partitioned into contexts. Next, the declarations
;; are collected for each context, and then compiled, before the expressions
;; inside the context are evaluated.
;;

(define (extract-single-element element)
  (define subelements (get-elements element))
  (unless (equal? 1 (length subelements))
    (error (format "Element ~a must contain a single subelement, not ~a"
                   (get-tag element) subelements)))
  (first subelements))

(define (extract-single-string element)
  (let ([e (extract-single-element element)])
    (unless (string? e)
      (error (format "Element ~a must contain a string, not ~a"
                     (get-tag element) e)))
    e))

(define (preprocess-decls decls)
  (define by-tag
    (for/fold ([by-tag (hash)])
              ([decl decls])
      (define key (first decl))
      (hash-set by-tag key (append (hash-ref by-tag key empty) (list decl)))))

  (list (txexpr 'includes empty (hash-ref by-tag 'include empty))
        (txexpr 'context-refs empty (hash-ref by-tag 'context-ref empty))
        (txexpr 'sorts empty (hash-ref by-tag 'sort  empty))
        (txexpr 'subsorts empty (hash-ref by-tag 'subsort empty))
        (txexpr 'vars empty (hash-ref by-tag 'var empty))
        (txexpr 'ops empty (hash-ref by-tag 'op empty))
        (txexpr 'rules empty (hash-ref by-tag 'rule empty))
        (txexpr 'assets empty (hash-ref by-tag 'asset empty))))

(define (preprocess-includes decls document)
  (define includes (get-elements (first decls)))
  (define processed-includes
    (for/list ([inc includes])
      (define-values (doc name)
        (documents:lookup-context document (attr-ref inc "ref")))
      (if doc
          `(include ((mode ,(attr-ref inc "mode"))
                     (document ,doc)
                     (context ,name)))
          `(include ((mode ,(attr-ref inc "mode"))
                     (context ,name))))))
  (cons (txexpr 'includes empty processed-includes)
        (rest decls)))

(define (preprocess-context-refs decls document)
  (define context-refs (get-elements (second decls)))
  (define processed-crefs
    (for/list ([cref context-refs])
      (define-values (doc name)
        (documents:lookup-context document (attr-ref cref "ref")))
      (if doc
          `(context-ref ((op ,(attr-ref cref "op"))
                         (document ,doc)
                         (context ,name)))
          `(context-ref ((op ,(attr-ref cref "op"))
                         (context ,name))))))
  (list* (first decls)
         (txexpr 'context-refs empty processed-crefs)
         (rest (rest decls))))

(define (process-context context-name context-elements document has-syntax-errors?)

  (define name-resolver (documents:context-name-resolver document))

  (define-values (contents decls)
    (extract-elements 'leibniz-decl context-elements))

  (define context-or-error
    (with-handlers ([contexts:exn:fail:leibniz? (λ (e) e)])
      (and (not has-syntax-errors?)
           (~> decls
               (map extract-single-element _)
               preprocess-decls
               (preprocess-includes document)
               (preprocess-context-refs document)
               (txexpr 'context `((id ,context-name)) _)
               (contexts:xexpr->context #f)
               contexts:add-implicit-declarations
               (contexts:compile-context name-resolver)))))

  (define (eval-contents element)
    (condd
     [(not (txexpr? element))
      element]
     #:do (define tag (get-tag element))

     [(equal? tag '+context)
      (leibniz-error "◊+context allowed only at top level"
                     element)]

     [(and (contexts:context? context-or-error)
           (member tag '(leibniz-check leibniz-eval)))
      (define source (attr-ref element 'source))
      (define source-tag (attr-ref element 'source-tag))
      (define label (attr-ref element 'label #f))
      (define expr (extract-single-element element))
      (define result
        (with-handlers ([contexts:exn:fail:leibniz? (λ (e) e)])
          (case tag
            [(leibniz-check)
             (contexts:check-asset context-or-error expr)]
            [(leibniz-eval)
             (contexts:eval-asset context-or-error expr)])))
      (match result
        [(exn:fail msg cont)
         (leibniz-error msg (format "◊+~a{~a}" source-tag source))]
        [else
         (asset->html result label)])]

     [else
      (txexpr tag
              (get-attrs element)
              (map eval-contents (get-elements element)))]))

  (define evaluated-contents
    (parameterize ([current-context-name-resolver name-resolver])
      ;; We cannot use map-elements here because it processes
      ;; elements inside to outside, so we use plain map and do
      ;; recursive traversal in eval-contents.
      (map eval-contents contents)))

  (define (row-for-context context-name text)
    (define-values (main-text context-column)
      (extract-elements 'context-column text))
    (list
     (txexpr* 'div '((class "row"))
              (txexpr 'div '((class "column context"))
                      (list*
                       (txexpr* 'hr '((style "border-top: dotted 1px; border-bottom: none; margin: 0px;")))
                       `(b "Context " (i ,context-name))
                       '(br)
                       (apply append
                              (map get-elements context-column))))
              (txexpr 'div '((class "column main")) main-text))))

  (condd
   ;; Syntax error in context definition
   [(not context-or-error)
    (values #f
            (row-for-context context-name
                             (cons '(context-column
                                     (span ((class "LeibnizError"))
                                           "not processed because of syntax errors in the document"
                                           (br)))
                                   evaluated-contents))
            document
            ;; Continue processing in order to check for syntax errors in the
            ;; whole document.
            #t)]
   ;; Error in processing the context after syntax checking
   [(contexts:exn:fail:leibniz? context-or-error)
    (match-define (contexts:exn:fail:leibniz msg cont decl) context-or-error)
    (values #f
            (row-for-context context-name
                             (list* `(context-column
                                      ,(leibniz-error msg decl)
                                      '(br))
                                    evaluated-contents))
            document
            ;; Don't continue processing because the following contexts
            ;; might include the current one which had errors.
            #f)]
   ;; No error
   #:do (define context context-or-error)
   #:do (define-values (cleaned-contents substitute-contexts)
          (extract-elements 'leibniz-substitute-context evaluated-contents))
   ;; Plain context definition
   [(empty? substitute-contexts)
    (values (contexts:context->xexpr context context-name)
            (row-for-context context-name cleaned-contents)
            (documents:add-context document context-name context)
            ;; Continue processing
            #t)]
   ;; Substituted context definition
   [else
    (unless (equal? 1 (length substitute-contexts))
      (error "more than one substitute context"))
    (define element (first substitute-contexts))
    (define source (attr-ref element 'source))
    (define expr (extract-single-element element))
    (define substitute-context-or-error
      (with-handlers ([contexts:exn:fail:leibniz? (λ (e) e)])
        (parameterize ([current-context-name-resolver name-resolver])
          (~> (contexts:eval-context-expr context expr)
              (contexts:compile-context name-resolver)))))
    (cond
      [(contexts:context? substitute-context-or-error)
       (define substitute-context substitute-context-or-error)
       (values (contexts:context->xexpr substitute-context context-name)
               (row-for-context context-name cleaned-contents)
               (documents:add-context document context-name substitute-context)
               ;; Continue processing
               #t)]
      [(contexts:exn:fail:leibniz? substitute-context-or-error)
       (match-define (contexts:exn:fail:leibniz msg cont decl)
                     substitute-context-or-error)
       (values #f
               (row-for-context context-name
                                (list* `(context-column
                                         ,(leibniz-error msg decl)
                                         '(br))
                                       evaluated-contents))
               document
               ;; Don't continue processing because the following contexts
               ;; might include the current one which had errors.
               #f)]
      ;; This shouldn't happen, as all errors to be signalled to the
      ;; document author should be of type exn:fail:leibniz. Consider
      ;; this section a debugging aid.
      [(exn:fail? substitute-context-or-error)
       (match-define (exn:fail msg cont) substitute-context-or-error)
       (values #f
               (row-for-context context-name
                                (list* `(context-column
                                         ,(leibniz-error msg '(unknown))
                                         '(br))
                                       evaluated-contents))
               document
               ;; Don't continue processing because the following contexts
               ;; might include the current one which had errors.
               #f)])]))

(define (root . elements)

  ;; Check for syntax errors. Contexts are not processed if any syntax errors
  ;; are found in the document.
  (define-values (document-elements syntax-error-markers)
    (extract-elements 'leibniz-syntax-error elements))
  (define has-syntax-errors? (not (empty? syntax-error-markers)))

  (define (non-context-element? e)
    (not ((has-tag? '+context) e)))

  (define-values (preamble first-context)
    (splitf-at document-elements non-context-element?))
  (define-values (library preamble-text)
    (partition (has-tag? 'document-ref) preamble))
  (define-values (document-with-imports reverse-import-list reverse-library-xml)
    (for/fold ([document documents:empty-document]
               [import-list empty]
               [library-xml empty])
              ([doc-ref library])
      (define name (attr-ref doc-ref "id"))
      (define filename (expand-user-path (attr-ref doc-ref "filename")))
      (define-values (library-document sha256-hex)
        (documents:read-xhtml-document filename))
      ;; This choice of library-url is good only for demos using
      ;; the Pollen Web server!
      (define library-url (string-append "/library/" sha256-hex ".html"))
      (values (documents:add-to-library document name
                                        library-document)
              (cons `(@ (a ((href ,library-url)) ,name)
                        '(br))
                    import-list)
              (cons `(document-ref ((id ,name) (sha256 ,sha256-hex)))
                    library-xml))))
  (define preamble-with-import-list
    (list
     (txexpr* 'div '((class "row"))
              (if (empty? reverse-import-list)
                  ""
                  (txexpr 'div '((class "column context"))
                          (list* '(b "Library documents:")
                                 '(br)
                                 (reverse reverse-import-list))))
              (txexpr 'div '((class "column main"))
                      preamble-text))))

  (define-values (contexts text)
    (let loop ([contexts empty]
               [text (list preamble-with-import-list)]
               [document document-with-imports]
               [todo first-context])
      (if (empty? todo)
          (values (reverse contexts)
                  (append* (reverse text)))
          (let*-values ([(contents next-context)
                         (splitf-at (rest todo) non-context-element?)]
                        [(declarations processed-contents extended-document continue?)
                         (process-context (extract-single-string (first todo))
                                          contents document has-syntax-errors?)])
            (if (not declarations)
                (loop contexts
                      (cons processed-contents text)
                      document
                      (if continue? next-context empty))
                (loop (cons declarations contexts)
                      (cons processed-contents text)
                      extended-document
                      (if continue? next-context empty)))))))

  (define errors
    (for/list ([i (range 1 (error-counter))])
      `((a ((href ,(format "#error~a" i)))
           ,(format "Error #~a" i))
        (br))))
  (define error-section
    (if (empty? errors)
        ""
        (txexpr 'div '((style "background:red")) (append* errors))))

  (define decoded-text
    (decode-elements (cons error-section text)
                     #:txexpr-elements-proc decode-paragraphs
                     #:string-proc smart-dashes))

  (txexpr* 'root empty
           (txexpr* 'leibniz empty
                    (txexpr 'leibniz-document empty
                            (cons (txexpr 'library empty
                                          (reverse reverse-library-xml))
                                  contexts)))
           (txexpr 'doc empty decoded-text)))

;; Import of documents

(define (+import document-name #:filename filename)
  `(document-ref ((id ,document-name) (filename ,filename))))

;; Context definition

(define +context (default-tag-function '+context))

;; Includes

(define (include* mode loc context-ref)
  `(@ (leibniz-decl (include ((mode ,mode) (ref ,context-ref))))
      (context-column ,mode "s: " (i ,context-ref) (br))))

(define-syntax (+use stx)
  (syntax-parse stx
    [(_ context-ref:string)
     #`(include* "use" #,(source-loc #'context-ref) context-ref)]))

(define-syntax (+extend stx)
  (syntax-parse stx
    [(_ context-ref:string)
     #`(include* "extend" #,(source-loc #'context-ref) context-ref)]))

(define (+context-ref symbol ref)
  `(leibniz-decl (context-ref ((op ,(symbol->string symbol))
                               (ref ,ref)))))

;; Sort and subsort declarations

(define-leibniz-parser +sort sort-or-subsort/p sort-decl sort-string
  (match sort-decl
    [`(sort ,sort-id)
     `(@ (leibniz-decl (sort ((id ,sort-string))))
         ,(leibniz-input (format-sort sort-id)))]
    [`(subsort ,sort-id-1 ,sort-id-2)
     `(@ (leibniz-decl (subsort ((subsort ,(symbol->string sort-id-1))
                                 (supersort ,(symbol->string sort-id-2)))))
         ,(leibniz-input (format-subsort sort-id-1 sort-id-2)))]))

;; Operator declarations

(define (arg->xexpr arg)
  (match arg
    [`(sort ,sort-id)
     `(sort ((id ,(symbol->string sort-id))))]
    [`(var ,name ,sort-id)
     `(var ((id ,(symbol->string name)) (sort ,(symbol->string sort-id))))]))

(define-leibniz-parser +op operator/p op-decl op-string
  (match-define `(op ,op-id ,arg-list ,v-sort) op-decl)
  `(@ (leibniz-decl (op ((id ,(symbol->string op-id)))
                        (arity ,@(map arg->xexpr arg-list))
                        (sort ((id ,(symbol->string v-sort))))))
      ,(leibniz-input (format-op (list op-id arg-list v-sort)))))

;; Var declarations

(define-leibniz-parser +var var/p var-decl var-string
  (match-define `(var ,var-id ,var-sort) var-decl)
  `(@ (leibniz-decl (var ((id ,(symbol->string var-id))
                          (sort ,(symbol->string var-sort)))))
      ,(leibniz-input (format-var var-id var-sort))))

;; Terms

(define (term->xexpr term)
  (match term
    [`(term-or-var ,op-id)
     `(term-or-var ((name ,(symbol->string op-id))))]
    [`(term ,op-id ,args)
     `(term ((op ,(symbol->string op-id))) ,@(map term->xexpr args))]
    [(list (and number-type (or 'integer 'rational 'floating-point)) x)
     `(,number-type ((value ,(number->string x))))]
    [(list 'string x)
     `(string ((value ,x)))]))

(module+ test
  (check-equal? (term->xexpr '(term-or-var foo))
                '(term-or-var ((name "foo"))))
  (check-equal? (term->xexpr '(term a-foo ()))
                '(term ((op "a-foo"))))
  (check-equal? (term->xexpr '(term a-foo ((integer 2) (string "abc"))))
                '(term ((op "a-foo")) (integer ((value "2"))) (string ((value "abc"))))))

(define-leibniz-parser-with-optional-label +term term/p term-decl term-string term-label
  (define term-expr (term->xexpr term-decl))
  (if term-label
      `(@ (leibniz-decl (asset ((id ,term-label)) ,term-expr))
          (leibniz-check ((source ,term-string) (source-tag "term") (label ,term-label))
                         ,term-expr))
      `(@ (leibniz-check ((source ,term-string) (source-tag "term"))
                         ,term-expr))))

(define-leibniz-parser +eval-term term/p term-decl term-string
  (define xexpr (term->xexpr term-decl))
  `(@ (leibniz-check ((source ,term-string) (source-tag "eval-term"))
                     ,xexpr)
      " ⇒ "
      (leibniz-eval ((source ,term-string) (source-tag "eval-term"))
                    ,xexpr)))

(define-leibniz-parser +trace term/p term-decl term-string
  (define xexpr (term->xexpr term-decl))
  `(@ (leibniz-check ((source ,term-string) (source-tag "trace"))
                     ,xexpr)
      (leibniz-eval ((source ,term-string) (source-tag "trace"))
                    (trace ,xexpr))))

(define-leibniz-parser +substitute-context term/p term-decl term-string
  (define xexpr (term->xexpr term-decl))
  `(@ (leibniz-check ((source ,term-string) (source-tag "substitute-context"))
                     ,xexpr)
      (leibniz-substitute-context ((source ,term-string)) ,xexpr)))

;; Rules

(define (group-clauses clauses)
  (for/fold ([vars empty]
             [condition #f])
            ([c clauses])
    (match c
      [`(var ,name ,sort)
       (values (cons `(var ((id ,(symbol->string name))
                            (sort ,(symbol->string sort))))
                     vars)
               condition)]
      [term
       (if condition
           (values vars (list 'term '_∧ (list condition term)))
           (values vars term))])))

(define-leibniz-parser-with-optional-label +rule rule/p rule-decl rule-string rule-label
  (match-define `(rule ,pattern ,replacement ,clauses) rule-decl)
  (define-values (vars condition) (group-clauses clauses))
  (define rule-expr `(rule (vars ,@vars)
                           (pattern ,(term->xexpr pattern))
                           ,(if condition
                                (list 'condition (term->xexpr condition))
                                '(condition))
                           (replacement ,(term->xexpr replacement))))
  (if rule-label
      `(@ (leibniz-decl (asset ((id ,rule-label)) ,rule-expr))
          (leibniz-check ((source ,rule-string) (source-tag "rule") (label ,rule-label))
                         ,rule-expr))
      `(@ (leibniz-decl ,rule-expr)
          (leibniz-check ((source ,rule-string) (source-tag "rule"))
                         ,rule-expr))))

;; Equations

(define-leibniz-parser-with-label +equation equation/p 
                                  equation-decl equation-string equation-label
  (match-define `(equation ,left ,right ,clauses) equation-decl)
  (define-values (vars condition) (group-clauses clauses))
  (define equation-expr `(equation (vars ,@vars)
                                   (left ,(term->xexpr left))
                                   ,(if condition
                                        (list 'condition
                                              (term->xexpr condition))
                                        '(condition))
                                   (right ,(term->xexpr right))))
  `(@ (leibniz-decl (asset ((id ,equation-label)) ,equation-expr))
      (leibniz-check ((source ,equation-string) (source-tag "equation")
                      (label ,equation-label))
                     ,equation-expr)))

;; Tests

(define-leibniz-parser +test rule/p rule-decl test-string
  (match-define `(rule ,pattern ,replacement ,clauses) rule-decl)
  (cond
    [(or (not (empty? clauses)))
     (let ((msg (format "Test may not contain vars or conditions: ~a" test-string)))
       `(@ (leibniz-error ,msg)
           (b ,msg)))]
    [else
     `(@ (leibniz-eval ((source ,test-string) (source-tag "test"))
                       (test (term ,(term->xexpr pattern))
                             (reduced-term ,(term->xexpr replacement)))))]))

;; Formatting

(define section (default-tag-function 'h2))
(define subsection (default-tag-function 'h3))
(define subsubsection (default-tag-function 'h4))
(define subsubsubsection (default-tag-function 'h5))
