#lang racket

;; Pollen interface 

(provide root
         +context +use +extend
         +sort
         +op
         +term
         +rule
         +test
         section subsection subsubsection subsubsubsection)

(require (submod txexpr safe)
         pollen/core
         pollen/decode
         pollen/tag
         pollen/unstable/typography
         megaparsack megaparsack/text
         "./condd.rkt"
         "./parser.rkt"
         (prefix-in contexts: "./contexts.rkt")
         (for-syntax syntax/parse)
         threading)

;; Parsing

(begin-for-syntax
  (define (source-loc stx)
    (list 'list
          (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx))))

(define (join-text . text-parts)
  (string-normalize-spaces (apply string-append text-parts)))

(define (parse-pollen-text parser text loc)
  (with-handlers ([exn:fail? (contexts:re-raise-exn (list loc))])
    (parse-result! (parse-string parser text))))

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

(define (process-context context-name context-elements)
  (define-values (contents decls)
    (splitf-txexpr (txexpr 'dummy empty context-elements)
                   (has-tag? 'leibniz-decl)))
  (define context
    (~> decls
        (map extract-single-element _)
        preprocess-decls
        (txexpr 'context empty _)
        contexts:xexpr->context
        contexts:add-implicit-declarations
        ;; contexts:compile-context
        ))
  (values (contexts:context->xexpr context context-name)
          (cons (txexpr* 'h3 empty "Context " context-name '(br) '(br))
                (get-elements contents))))

(define (root . elements)

  (define (not-context-element? e)
    (not ((has-tag? 'leibniz-new-context) e)))

  (define-values (preamble first-context)
    (splitf-at elements not-context-element?))

  (define-values (contexts document)
    (let loop ([contexts empty]
               [document (list preamble)]
               [todo first-context])
      (if (empty? todo)
          (values (reverse contexts)
                  (append* (reverse document)))
          (let*-values ([(contents next-context)
                         (splitf-at (rest todo) not-context-element?)]
                        [(declarations processed-contents)
                         (process-context (extract-single-string (first todo))
                                          contents)])
            (loop (cons declarations contexts)
                  (cons processed-contents document)
                  next-context)))))

  (define decoded-document
    (decode-elements document
                     #:txexpr-elements-proc decode-paragraphs
                     #:string-proc smart-dashes))

  (txexpr* 'root empty
           (txexpr* 'leibniz empty (txexpr 'leibniz-contexts empty contexts))
           (txexpr 'doc empty decoded-document)))

;; Context definition

(define +context (default-tag-function 'leibniz-new-context))

;; Includes

(define (include* mode loc context-ref)
  `(@ (leibniz-decl (include ((mode ,mode) (ref ,context-ref))))
      (b ,mode ": ") (i ,context-ref) (br)))

(define-syntax (+use stx)
  (syntax-parse stx
    [(_ context-ref:string)
     #`(include* "use" #,(source-loc #'context-ref) context-ref)]))

(define-syntax (+extend stx)
  (syntax-parse stx
    [(_ context-ref:string)
     #`(include* "extend" #,(source-loc #'context-ref) context-ref)]))

;; Sort and subsort declarations

(define (sort* loc sort-string)
  (define sort-decl (parse-pollen-text sort-or-subsort/p sort-string loc))
  (match sort-decl
    [`(sort ,sort-id)
     `(@ (leibniz-decl (sort ((id ,sort-string))))
         (i ,sort-string))]
    [`(subsort ,sort-id-1 ,sort-id-2)
     `(@ (leibniz-decl (subsort ((subsort ,(symbol->string sort-id-1))
                                 (supersort ,(symbol->string sort-id-2)))))
         (i ,sort-string))]))

(define-syntax (+sort stx)
  (syntax-parse stx
    [(_ first-str:string more-strs:string ...)
     #`(sort* #,(source-loc #'first-str)
              (join-text first-str more-strs ...))]))

;; Operator declarations

(define (arg->xexpr arg)
  (match arg
    [`(sort ,sort-id)
     `(sort ((id ,(symbol->string sort-id))))]
    [`(var ,name ,sort-id)
     `(var ((id ,(symbol->string name)) (sort ,(symbol->string sort-id))))]))

(define (op* loc op-string)
  (define op-decl (parse-pollen-text operator/p op-string loc))
  (match-define `(op ,op-id ,arg-list ,result-sort)  op-decl)
  `(@ (leibniz-decl (op ((id ,(symbol->string op-id)))
                        (arity ,@(for/list ([arg arg-list])
                                   (arg->xexpr arg)))
                        (sort ((id ,(symbol->string result-sort))))))
      (i ,op-string)))

(define-syntax (+op stx)
  (syntax-parse stx
    [(_ first-str:string more-strs:string ...)
     #`(op* #,(source-loc #'first-str)
            (join-text first-str more-strs ...))]))

;; Terms

(define (term->xexpr term)
  (match term
    [`(term-or-var ,op-id)
     `(term-or-var ((name ,(symbol->string op-id))))]
    [`(term ,op-id ,args)
     `(term ((op ,(symbol->string op-id))) ,@(map term->xexpr args))]
    [(list (and number-type (or 'integer 'rational 'floating-point)) x)
     `(,number-type ((value ,(number->string x))))]))

(define (term* loc term-string)
  (define term-decl (parse-pollen-text term/p term-string loc))
  `(@ (leibniz-check (term->xexpr term-decl))
      (i ,term-string)))

(define-syntax (+term stx)
  (syntax-parse stx
    [(_ first-str:string more-strs:string ...)
     #`(term* #,(source-loc #'first-str)
              (join-text first-str more-strs ...))]))

;; Rules

(define (group-clauses clauses)
  (for/fold ([vars empty]
             [condition #f])
            ([c clauses])
    (match c
      [`(var ,name ,sort)
       (values (cons `(var ((id ,(symbol->string name)) (sort ,(symbol->string sort)))) vars)
               condition)]
      [term
       (if condition
           (values vars (list 'term '_∧ (list condition term)))
           (values vars term))])))

(define (rule* loc type-label rule-string)
  (define rule-decl (parse-pollen-text rule/p rule-string loc))
  (match-define `(rule ,pattern ,replacement ,clauses) rule-decl)
  (define-values (vars condition) (group-clauses clauses))
  `(@ (leibniz-decl (,type-label (vars ,@vars)
                                 (pattern ,(term->xexpr pattern))
                                 ,(if condition
                                      (list 'condition (term->xexpr condition))
                                      '(condition))
                                 (replacement ,(term->xexpr replacement))))
      (i ,rule-string)))

(define-syntax (+rule stx)
  (syntax-parse stx
    [(_ first-str:string more-strs:string ...)
     #`(rule* #,(source-loc #'first-str)
              'rule
              (join-text first-str more-strs ...))]))

(define (test* loc test-string)
  (define rule-decl (parse-pollen-text rule/p test-string loc))
  (match-define `(rule ,pattern ,replacement ,clauses) rule-decl)
  (cond
    [(or (not (empty? clauses)))
     (let ((msg (format "Test may not contain vars or conditions: ~a" test-string)))
       `(@ (leibniz-error ,msg)
           (b ,msg)))]
    [else
     `(@ (leibniz-eval (test ,(term->xexpr pattern)
                             ,(term->xexpr replacement)))
         (i ,test-string))]))

(define-syntax (+test stx)
  (syntax-parse stx
    [(_ first-str:string more-strs:string ...)
     #`(test* #,(source-loc #'first-str)
              (join-text first-str more-strs ...))]))

;; Formatting

(define section (default-tag-function 'h2))
(define subsection (default-tag-function 'h3))
(define subsubsection (default-tag-function 'h4))
(define subsubsubsection (default-tag-function 'h45))
