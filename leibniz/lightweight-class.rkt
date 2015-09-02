#lang racket

(provide define-class)

; Define a very lightweight class. A class definition generates an
; equivalent transparent struct plus a function corresponding to
; each method. Inside the methods, fields can be accessed by name,
; rather than having to use the lengthy field accessor functions
; defined by Racket.

(require (for-syntax racket/syntax syntax/parse syntax/stx))

(define-syntax (define-class stx)
  (syntax-parse stx
    [(_ class-name:id
        ((~literal field) field-name:id ...)
        ((~literal define)
         (method-name:id method-arg:id ...) body:expr ...) ...)
     (with-syntax ([obj-arg
                    (generate-temporary #'class-name)]
                   [((temp-arg ...) ...)
                    (stx-map generate-temporaries #'((method-arg ...) ...))]
                   [(accessor ...)
                    (stx-map (λ (x) (format-id #'class-name
                                               "~a-~a" #'class-name x))
                             #'(field-name ...))]
                   [this (datum->syntax stx 'this)])
       #'(begin
           (struct class-name [field-name ...] #:transparent)
           (define (method-name obj-arg temp-arg ...)
             (let ([this obj-arg]
                   [field-name (accessor obj-arg)] ...)
               (let ([method-name (λ (method-arg ...)
                                    (method-name obj-arg method-arg ...))] ...)
                 (let ([method-arg temp-arg] ...)
                   body ...))))
           ...))]))


(module* test #f
  (require rackunit)
  
  (define-class foo
    (field a b)
    (define (bar x)
      (* a b x))
    (define (baz)
      (bar 1))
    (define (get)
      this)
    (define (id1 a)
      a)
    (define (id2 this)
      this))

  (define a-foo (foo 2 3))
  (check-equal? (bar a-foo 42) 252)
  (check-equal? (baz a-foo) 6)
  (check-eq? (get a-foo) a-foo)
  (check-eq? (id1 a-foo 42) 42)
  (check-eq? (id2 a-foo 42) 42))
