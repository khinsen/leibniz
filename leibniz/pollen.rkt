#lang racket

;; Pollen interface 

(provide root
         context
         type
         op)

(require txexpr
         pollen/core
         pollen/decode
         pollen/unstable/typography
         megaparsack megaparsack/text
         "./condd.rkt"
         "./parser.rkt"
         (only-in "./documents.rkt" re-raise-exn)
         (for-syntax syntax/parse))

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
  (with-handlers ([exn:fail? (re-raise-exn (list loc))])
    (parse-result! (parse-string parser text))))


(define (root . elements)
  (define contexts (select* 'leibniz (txexpr 'root empty elements)))
  (define doc
    (decode-elements elements
                     #:txexpr-proc (λ (x) (if (equal? (get-tag x) 'leibniz) "" x))
                     #:txexpr-elements-proc decode-paragraphs
                     #:string-proc smart-dashes))
  (txexpr* 'root empty
           (txexpr 'leibniz empty (txexpr 'leibniz-document empty contexts))
           (txexpr 'doc empty doc)))

(define (context name . elements)
  (define leibniz (select* 'leibniz (txexpr 'context empty elements)))
  (define doc
    (decode-elements elements
                     #:txexpr-proc (λ (x) (if (equal? (get-tag x) 'leibniz) "" x))))
  `(@ (leibniz (context ((id ,name)) ,@leibniz))
      (h3 "Context " ,name) (br) (br) ,@doc))

(define (type* loc type-string)
  (define type-decl (parse-pollen-text sort-or-subsort/p type-string loc))
  (match type-decl
    [`(sort ,sort-id)
     `(@ (leibniz (type ((id ,type-string))))
         (i ,type-string))]
    [`(subsort ,sort-id-1 ,sort-id-2)
     `(@ (leibniz (subtype ((subsort ,type-string) (supersort ,type-string))))
         (i ,type-string))]))

(define-syntax (type stx)
  (syntax-parse stx
    [(_ first-type-str:string more-type-strs:string ...)
     #`(type* #,(source-loc #'first-type-str)
              (join-text first-type-str more-type-strs ...))]))

(define (op* loc op-string)
  (define op-decl (parse-pollen-text operator/p op-string loc))
  (match-define `(op ,op-id ,arg-list ,result-type)  op-decl)
  `(@ (leibniz (op ((id ,(symbol->string op-id)))
                   (arity ,(for/splice ([arg arg-list]) `(type ((id ,(symbol->string arg))))))
                   (type ((id ,(symbol->string result-type))))))
      (i ,op-string)))

(define-syntax (op stx)
  (syntax-parse stx
    [(_ first-op-str:string more-op-strs:string ...)
     #`(op* #,(source-loc #'first-op-str)
            (join-text first-op-str more-op-strs ...))]))
