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
                     "./lang/parser.rkt"
                     megaparsack megaparsack/text
                     data/monad
                     data/applicative)
         "./documents.rkt"
         racket/trace)

(begin-for-syntax
  (define (source-loc stx)
    (list 'list
          (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx))))

; Contexts

(begin-for-syntax
  (define-splicing-syntax-class arg-or-kw-arg
    (pattern (~seq kw:keyword arg:expr)
             #:with arg-in-list #'(kw arg))
    (pattern arg:expr
             #:with arg-in-list #'((in-context arg)))))

(define-syntax (in-context stx)
  (syntax-parse stx
    [(_ ((~literal sort) sort-decl:str))
     #`(parsed-declaration (quote #,(parse-result!
                                     (parse-syntax-string (syntax/p sort-or-subsort/p)
                                                          #'sort-decl))))]
    [(_ ((~literal op) op-decl:str))
     #`(parsed-declaration (quote #,(parse-result!
                                     (parse-syntax-string (syntax/p operator/p)
                                                          #'op-decl))))]
    [(_ ((~literal term) term-expr:str))
     #`(parsed-term (quote #,(parse-result!
                              (parse-syntax-string (syntax/p (to-eof/p term/p))
                                                   #'term-expr))))]
    [(_ ((~literal rule) rule-expr:str))
     #`(parsed-rule (quote #,(parse-result!
                              (parse-syntax-string (syntax/p rule/p)
                                                   #'rule-expr))))]
    [(_ ((~literal equation) equation-expr:str))
     #`(parsed-equation (quote #,(parse-result!
                              (parse-syntax-string (syntax/p equation/p)
                                                   #'equation-expr))))]
    [(_ (element args:arg-or-kw-arg ...))
     #`(element #,@(apply append (map syntax-e (syntax-e #'(args.arg-in-list ...)))))]
    [(_ any)
     #'any]))

(define-syntax (context stx)
  (syntax-parse stx
    [(_ name body ...)
     #'(begin (displayln name)
              (in-context body) ...)]))

; Declarations (-> signatures)

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

;; (define-syntax-parameter sort
;;   (λ (stx)
;;     (raise-syntax-error 'sort "sort keyword used outside context" stx)))

;; (define-syntax-parameter op
;;   (λ (stx)
;;     (raise-syntax-error 'op "op keyword used outside context" stx)))

; Sorts

;; (define-syntax (sort stx)
;;   (let* ([sort-expr (syntax-parse stx
;;                       [(_ sort-expr:str) #'sort-expr])]
;;          [parsed-expr (parse-result!
;;                        (parse-syntax-string (syntax/p sort-or-subsort/p)
;;                                             sort-expr))]
;;          [loc (datum->syntax stx (source-loc sort-expr))]
;;          [leibniz-ref (datum->syntax stx 'leibniz)])
;;     #`(begin (set! #,leibniz-ref (add-declaration #,leibniz-ref (quote #,parsed-expr) #,loc))
;;              (format-sort-expr (quote #,parsed-expr)))))

; operators

;; (define-syntax (op stx)
;;   (let* ([op-expr (syntax-parse stx
;;                     [(_ op-expr:str) #'op-expr])]
;;          [parsed-expr (parse-result!
;;                        (parse-syntax-string (syntax/p operator/p)
;;                                             op-expr))]
;;          [loc (datum->syntax stx (source-loc op-expr))]
;;          [leibniz-ref (datum->syntax stx 'leibniz)])
;;     #`(begin (set! #,leibniz-ref (add-declaration #,leibniz-ref (quote #,parsed-expr) #,loc))
;;              (format-op-expr (quote #,parsed-expr)))))


; Terms

;; (define-syntax (term stx)
;;   (let* ([term-expr (syntax-parse stx
;;                       [(_ term-expr:str) #'term-expr])]
;;          [parsed-expr (parse-result!
;;                        (parse-syntax-string (syntax/p (to-eof/p term/p))
;;                                             term-expr))])
;;     #`(format-term-expr (quote #,parsed-expr))))


; Rules

;; (define-syntax (rule stx)
;;   (let* ([rule-expr (syntax-parse stx
;;                       [(_ rule-expr:str) #'rule-expr])]
;;          [parsed-expr (parse-result!
;;                        (parse-syntax-string (syntax/p rule/p)
;;                                             rule-expr))])
;;     #`(format-rule-expr (quote #,parsed-expr))))


; Equations

;; (define-syntax (equation stx)
;;   (let* ([equation-expr (syntax-parse stx
;;                       [(_ equation-expr:str) #'equation-expr])]
;;          [parsed-expr (parse-result!
;;                        (parse-syntax-string (syntax/p equation/p)
;;                                             equation-expr))]
;;          [leibniz-ref (datum->syntax stx 'leibniz)])
;;     #`(begin (set! #,leibniz-ref (process-declarations #,leibniz-ref))
;;              (format-equation-expr (quote #,parsed-expr)))))
