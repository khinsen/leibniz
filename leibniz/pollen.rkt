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
         "./parser.rkt")


(define (parse-pollen-text parser text)
  (parse-result!
   (parse-string parser (string-normalize-spaces (apply string-append text)))))

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

(define (type type-string)
  `(@ (leibniz (type ((id ,type-string)))) (i ,type-string)))

(define (op . op-strings)
  (define op-decl (parse-pollen-text operator/p op-strings))
  (match-define `(op ,op-id ,arg-list ,result-type)  op-decl)
  (displayln (format "~a ~a ~a" op-id arg-list result-type))
  `(@ (leibniz (op ((id ,(symbol->string op-id)))
                   (arity ,(for/splice ([arg arg-list]) `(type ((id ,(symbol->string arg))))))
                   (type ((id ,(symbol->string result-type))))))
      (i ,@op-strings)))
