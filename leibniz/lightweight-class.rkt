#lang racket

(provide define-class send)

; Define a very lightweight class. A class definition generates an
; equivalent transparent struct plus a function corresponding to
; each method. Inside the methods, fields can be accessed by name,
; rather than having to use the lengthy field accessor functions
; defined by Racket.

(require (for-syntax racket/syntax
                     syntax/parse
                     syntax/stx)
         racket/splicing
         racket/generic)

;; (define-syntax-parameter send #f)
(define-generics lightweight-object
  (send-msg lightweight-object method-symbol . args))

(define-syntax-rule (send object method-symbol arg ...)
  (send-msg object (quote method-symbol) arg ...))

(define-syntax (define-class stx)
  (syntax-parse stx
    [(_ class-name:id
        ((~literal field) field-name:id ...)
        (~optional (~seq #:write-proc my-write-proc:expr))
        ((~literal define)
         (method-name:id method-arg:id ...) body:expr ...) ...)
     (with-syntax* ([obj-arg
                     (generate-temporary #'class-name)]
                    [((temp-arg ...) ...)
                     (stx-map generate-temporaries #'((method-arg ...) ...))]
                    [(accessor ...)
                     (stx-map (λ (x) (format-id #'class-name
                                                "~a-~a" #'class-name x))
                              #'(field-name ...))]
                    [this (datum->syntax stx 'this)]
                    [(ext-method-name ...)
                     (generate-temporaries #'(method-name ...))])
       (let ([custom-write-impl
              (if (attribute my-write-proc)
                  #'(#:methods gen:custom-write
                     [(define write-proc my-write-proc)])
                  #'())])
         #`(begin
             (define (method-name obj-arg temp-arg ...)
               (let ([this obj-arg]
                     [field-name (accessor obj-arg)] ...)
                 (let ([ext-method-name method-name] ...)
                   (let ([method-name (λ (method-arg ...)
                                        (method-name obj-arg method-arg ...))]
                         ...)
                     (let ([method-arg temp-arg] ...)
                       body ...)))))
             ...
             (struct class-name [field-name ...] #:transparent
               #:methods gen:lightweight-object
               [(define (send-msg object symbol . args)
                  (apply (case symbol
                           [(method-name) method-name] ...)
                         (list* object args)))]
               #,@custom-write-impl))))]))

(module* test #f
  (require rackunit)
  
  (define-class foo
    (field a b)
    #:write-proc *write*
    (define (bar x)
      (* a b x))
    (define (baz)
      (bar 1))
    (define (get)
      this)
    (define (get-a)
      a)
    (define (id1 a)
      a)
    (define (id2 this)
      this)
    (define (add-as other)
      (+ a (send other get-a)))
    (define (*write* p w)
      (write-string (format "<foo ~a ~a>" a b) p)))


  (define a-foo (foo 2 3))
  (define another-foo (foo 10 20))
  (check-equal? (bar a-foo 42) 252)
  (check-equal? (baz a-foo) 6)
  (check-eq? (get a-foo) a-foo)
  (check-eq? (get-a a-foo) 2)
  (check-eq? (send a-foo get-a) 2)
  (check-eq? (id1 a-foo 42) 42)
  (check-eq? (id2 a-foo 42) 42)
  (check-eq? (add-as a-foo another-foo)
             (+ (get-a a-foo) (get-a another-foo))))
