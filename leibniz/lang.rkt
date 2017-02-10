#lang racket

(provide (all-from-out scribble/doclang
                       scribble/base)
         sort op term rule equation
         empty-document)

(require scribble/doclang
         scribble/base
         (for-syntax syntax/parse
                     "./lang/parser.rkt"
                     megaparsack megaparsack/text
                     data/monad
                     data/applicative)
         "./documents.rkt")

(begin-for-syntax
  (define (source-loc stx)
    (list 'list
          (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx))))

; Sorts

(define-syntax (sort stx)
  (let* ([sort-expr (syntax-parse stx
                      [(_ sort-expr:str) #'sort-expr])]
         [parsed-expr (parse-result!
                       (parse-syntax-string (syntax/p sort-or-subsort/p)
                                            sort-expr))]
         [loc (datum->syntax stx (source-loc sort-expr))]
         [leibniz-ref (datum->syntax stx 'leibniz)])
    #`(begin (set! #,leibniz-ref (add-declaration #,leibniz-ref (quote #,parsed-expr) #,loc))
             (format-sort-expr (quote #,parsed-expr)))))

(define (format-sort symbol)
  (bold (italic (symbol->string symbol))))

(define (format-sort-expr parsed-sort-expr)
  (nonbreaking
   (match parsed-sort-expr
     [(list 'sort sort-symbol)
      (format-sort sort-symbol)]
     [(list 'subsort sort-symbol-1 sort-symbol-2)
      (list (format-sort sort-symbol-1)
            " ⊆ "  ; MEDIUM MATHEMATICAL SPACE ; SUBSET OF OR EQUAL TO
            (format-sort sort-symbol-2))])))

; Operators

(define-syntax (op stx)
  (let* ([op-expr (syntax-parse stx
                    [(_ op-expr:str) #'op-expr])]
         [parsed-expr (parse-result!
                       (parse-syntax-string (syntax/p operator/p)
                                            op-expr))]
         [loc (datum->syntax stx (source-loc op-expr))]
         [leibniz-ref (datum->syntax stx 'leibniz)])
    #`(begin (set! #,leibniz-ref (add-declaration #,leibniz-ref (quote #,parsed-expr) #,loc))
             (format-op-expr (quote #,parsed-expr)))))

(define (format-prefix-op symbol)
  (italic (symbol->string symbol)))

(define (format-infix-op symbol)
  (italic (symbol->string symbol)))

(define (format-op-expr parsed-op-expr)
  (nonbreaking
   (match parsed-op-expr
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

; Terms

(define-syntax (term stx)
  (let* ([term-expr (syntax-parse stx
                      [(_ term-expr:str) #'term-expr])]
         [parsed-expr (parse-result!
                       (parse-syntax-string (syntax/p (to-eof/p term/p))
                                            term-expr))])
    #`(format-term-expr (quote #,parsed-expr))))

(define (format-term-expr parsed-term-expr)
  (nonbreaking) (format "~a" parsed-term-expr))

; Rules

(define-syntax (rule stx)
  (let* ([rule-expr (syntax-parse stx
                      [(_ rule-expr:str) #'rule-expr])]
         [parsed-expr (parse-result!
                       (parse-syntax-string (syntax/p rule/p)
                                            rule-expr))])
    #`(format-rule-expr (quote #,parsed-expr))))

(define (format-rule-expr parsed-rule-expr)
  (nonbreaking (format "~a" parsed-rule-expr)))

; Equations

(define-syntax (equation stx)
  (let* ([equation-expr (syntax-parse stx
                      [(_ equation-expr:str) #'equation-expr])]
         [parsed-expr (parse-result!
                       (parse-syntax-string (syntax/p equation/p)
                                            equation-expr))]
         [leibniz-ref (datum->syntax stx 'leibniz)])
    #`(begin (set! #,leibniz-ref (process-declarations #,leibniz-ref))
             (format-equation-expr (quote #,parsed-expr)))))

(define (format-equation-expr parsed-equation-expr)
  (nonbreaking (format "~a" parsed-equation-expr)))
