#lang racket

;; Pollen interface 

(provide root
         +context +use +extend
         +sort
         +op
         +var
         +term +eval-term
         +rule
         +test
         section subsection subsubsection subsubsubsection)

(require (submod txexpr safe)
         pollen/core
         pollen/decode
         pollen/tag
         pollen/unstable/typography
         megaparsack megaparsack/text
         data/either
         "./condd.rkt"
         "./parser.rkt"
         "./formatting.rkt"
         (prefix-in contexts: "./contexts.rkt")
         (prefix-in documents: "./documents.rkt")
         (for-syntax syntax/parse)
         xml
         threading)

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

(define (with-parsed-text tag parser text-parts fn)
  (define text (string-normalize-spaces (apply string-append text-parts)))
  (define result (parse-string (to-eof/p parser) text))
  (if (failure? result)
      (match (from-failure #f result)
        [(message loc unexpected expected)
         (match loc
           [(srcloc source line column position span)
            (leibniz-syntax-error tag text column unexpected expected)])])
      (fn (from-success #f result) text)))

(define-syntax (define-leibniz-parser stx)
  (syntax-parse stx
    [(_ tag:id parser:expr decl text body-exprs ...)
     #'(define (tag . text-parts)
         (with-parsed-text (quote tag) parser text-parts
           (λ (decl text)
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

(define ((has-tag? tag) element)
  (and (txexpr? element)
       (equal? (get-tag element) tag)))

(define (preprocess-decls decls)
  (define by-tag
    (for/fold ([by-tag (hash)])
              ([decl decls])
      (define key (first decl))
      (hash-set by-tag key (append (hash-ref by-tag key empty) (list decl)))))

  (list (txexpr 'includes empty (hash-ref by-tag 'include empty))
        (txexpr 'sorts empty (hash-ref by-tag 'sort  empty))
        (txexpr 'subsorts empty (hash-ref by-tag 'subsort empty))
        (txexpr 'vars empty (hash-ref by-tag 'var empty))
        (txexpr 'ops empty (hash-ref by-tag 'op empty))
        (txexpr 'rules empty (hash-ref by-tag 'rule empty))
        (txexpr 'assets empty (hash-ref by-tag 'asset empty))))

(define (process-context context-name context-elements document has-syntax-errors?)

  (define-values (contents decls)
    (splitf-txexpr (txexpr 'dummy empty context-elements)
                   (has-tag? 'leibniz-decl)))

  (define context-or-error
    (with-handlers ([contexts:exn:fail:leibniz? (λ (e) e)])
      (and (not has-syntax-errors?)
           (~> decls
               (map extract-single-element _)
               preprocess-decls
               (txexpr 'context empty _)
               contexts:xexpr->context
               contexts:add-implicit-declarations
               (contexts:compile-context (documents:context-name-resolver document))))))

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
      (define expr (extract-single-element element))
      (define result
        (with-handlers ([exn:fail? (λ (e) e)])
          (case tag
            [(leibniz-check)
             (contexts:check-asset context-or-error expr)]
            [(leibniz-eval)
             (contexts:eval-asset context-or-error expr)])))
      (match result
        [(exn:fail msg cont)
         (leibniz-error msg (format "◊+~a{~a}" source-tag source))]
        [else
         (asset->html result)])]

     [else
      (txexpr tag
              (get-attrs element)
              (map eval-contents (get-elements element)))]))

  (define (row-for-context context-name text)
    (define-values (main-text context-column)
      (splitf-txexpr (txexpr 'dummy empty text)
                     (has-tag? 'context-column)))
    (list
     (txexpr* 'hr '((style "border-top: dotted 1px;"))
              (txexpr* 'div '((class "row"))
                       (txexpr 'div '((class "column context"))
                               (list*
                                (txexpr* 'h4 empty
                                         "Context " context-name)
                                (apply append
                                       (map get-elements context-column))))
                       (txexpr 'div '((class "column main"))
                               (get-elements main-text))))))

  (cond
    [(contexts:context? context-or-error)
     (define context context-or-error)
     (values (contexts:context->xexpr context context-name)
             (row-for-context context-name
                              ;; We cannot use map-elements here because it
                              ;; processes elements inside to outside, so we
                              ;; use plain map and do recursive traversal
                              ;; in eval-contents.
                              (map eval-contents (get-elements contents)))
             (documents:add-context document context-name context))]
    [(contexts:exn:fail:leibniz? context-or-error)
     (match-define (contexts:exn:fail:leibniz msg cont decl) context-or-error)
     (values #f
             (row-for-context context-name
                              (cons `(context-column
                                      ,(leibniz-error msg decl))
                                    (map eval-contents
                                         (get-elements contents))))
             document)]
    [else
     (values #f
             (row-for-context context-name
                              (cons '(context-column
                                      (span ((class "LeibnizError"))
                                            "not processed because of syntax errors in the document"))
                                    (map eval-contents
                                         (get-elements contents))))
             document)]))

(define (root . elements)

  ;; Check for syntax errors. Contexts are not processed if any syntax errors
  ;; are found in the document.
  (define-values (document-root syntax-error-markers)
    (splitf-txexpr (txexpr 'root empty elements)
                   (has-tag? 'leibniz-syntax-error)))
  (define document-elements (get-elements document-root))
  (define has-syntax-errors? (not (empty? syntax-error-markers)))

  (define (non-context-element? e)
    (not ((has-tag? '+context) e)))

  (define-values (preamble first-context)
    (splitf-at document-elements non-context-element?))

  (define-values (contexts text)
    (let loop ([contexts empty]
               [text (list preamble)]
               [document documents:empty-document]
               [todo first-context])
      (if (empty? todo)
          (values (reverse contexts)
                  (append* (reverse text)))
          (let*-values ([(contents next-context)
                         (splitf-at (rest todo) non-context-element?)]
                        [(declarations processed-contents extended-document)
                         (process-context (extract-single-string (first todo))
                                          contents document has-syntax-errors?)])
            (if (not declarations)
                (loop contexts
                      (cons processed-contents text)
                      document
                      next-context)
                (loop (cons declarations contexts)
                      (cons processed-contents text)
                      extended-document
                      next-context))))))

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
           (txexpr* 'leibniz empty (txexpr 'leibniz-contexts empty contexts))
           (txexpr 'doc empty decoded-text)))

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
     `(,number-type ((value ,(number->string x))))]))

(define-leibniz-parser +term term/p term-decl term-string
  `(@ (leibniz-check ((source ,term-string) (source-tag "term"))
                     ,(term->xexpr term-decl))))

(define-leibniz-parser +eval-term term/p term-decl term-string
  (define xexpr (term->xexpr term-decl))
  `(@ (leibniz-check ((source ,term-string) (source-tag "eval-term"))
                     ,xexpr)
      " ⇒ "
      (leibniz-eval ((source ,term-string) (source-tag "eval-term"))
                    ,xexpr)))

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

(define-leibniz-parser +rule rule/p rule-decl rule-string
  (match-define `(rule ,pattern ,replacement ,clauses) rule-decl)
  (define-values (vars condition) (group-clauses clauses))
  (define rule-expr `(rule (vars ,@vars)
                           (pattern ,(term->xexpr pattern))
                           ,(if condition
                                (list 'condition (term->xexpr condition))
                                '(condition))
                           (replacement ,(term->xexpr replacement))))
  `(@ (leibniz-decl ,rule-expr)
      (leibniz-check ((source  ,rule-string) (source-tag "rule"))
                     ,rule-expr)))

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
