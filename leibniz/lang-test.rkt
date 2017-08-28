#lang racket

;;; Tests for #lang leibniz

;;
;; Turn a text string into a Leibniz module and
;; return the value of its document.
;;
;; The module-massaging magic is due to Matthew Flatt:
;; https://groups.google.com/d/msg/racket-users/6nnpbrkjAck/tgpgbKzBAQAJ
;;


(define (leibniz-document leibniz-code)
  (parameterize ([current-namespace (make-base-namespace)])
    ;; Read module as a syntax object:
    (define mod-form
      (parameterize ([read-accept-lang #t]
                     [read-accept-reader #t]
                     [port-count-lines-enabled #t])
        (read-syntax
         "test-module"
         (open-input-string (string-join (list "#lang leibniz\n" leibniz-code))))))

    ;; Declare the module:
    (parameterize ([current-module-declare-name 
                    ;; declare as name 'demo:
                    (make-resolved-module-path 'test-module)])
      (eval mod-form))

    ;; Return the value of identifier "leibniz"
    (dynamic-require ''test-module 'leibniz)))


(module+ test
  (require rackunit
           threading
           leibniz/documents)

  (define test-module-source
#<<---
@context["test-context"]{
@sort{A}
@var{any-A:A}
}
---
   )
  (define test-document (leibniz-document test-module-source))

  (check-equal? (~> test-document
                    (get-context "test-context")
                    (hash-remove 'locs))
                (~> empty-document
                    (add-context-from-source
                     "test-context"
                     (list (cons '(sort A) #f)
                           (cons '(var any-A A) #f)
                           ;; (cons '() #f)
                           ;; (cons '() #f)
                           ;; (cons '() #f)
                           ))
                    (get-context "test-context")
                    (hash-remove 'locs))))
