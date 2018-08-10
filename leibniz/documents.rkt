#lang racket

(provide empty-document
         add-to-library
         add-context
         get-context
         context-name-resolver)

(require (prefix-in builtins: "./builtin-contexts.rkt")
         (prefix-in contexts: "./contexts.rkt")
         "./lightweight-class.rkt"
         threading)

(module+ test
  (require rackunit))

;;
;; A document is a collection of contexts that can refer to each other by name.
;; Each document also keeps a library of other documents to whose contexts
;; its own contexts can refer. The dependency graph is acyclic by construction.
;;
(define-class document

  (field contexts order library library-refs)
  ;; contexts: a hash mapping context names to hashes with keys
  ;;           'includes 'sorts 'ops 'vars 'rules 'assets,
  ;;           values are lists/sets/hashes of the declarations in
  ;;            each category
  ;; order: a list of context names in the inverse order of definition
  ;; library: a hash mapping document names to documents
  ;; library-refs: a hash mapping document names to external references
  ;;               (filenames for now, later maybe DOIs or hashes)

  ;; Add another document to the library.
  (define (add-to-library name library-document external-ref)
    (document contexts order
              (hash-set library name library-document)
              (hash-set library-refs name external-ref)))

  ;; Add a context defined by a context data structure.
  (define (add-context name cntxt)
    (document (hash-set contexts name cntxt)
              (cons name order)
              library
              library-refs))

  ;; Retrieve a context by name
  (define (get-document-and-context name)
    (define elements (map string-trim (string-split name "/")))
    (case (length elements)
      [(1)
       (unless (hash-has-key? contexts (first elements))
         (error (format "no context named ~a" name)))
       (values this (hash-ref contexts (first elements)))]
      [(2)
       (unless (hash-has-key? library (first elements))
         (error (format "no library named ~a" (first elements))))
       (define library-doc (hash-ref library (first elements)))
       (values library-doc (send library-doc get-context (second elements)))]
      [else
       (error (format "illegal context specification ~a" name))]))

  (define (get-context name)
    (define-values (document context) (get-document-and-context name))
    context)

  (define (context-name-resolver)
    (λ (name) (get-context name))))

;; A document containing the builtin contexts

(define builtins
  (~> (document (hash) empty (hash) (hash))
      (add-context "truth"
                   (contexts:make-builtin-context
                    empty
                    builtins:truth-signature
                    builtins:truth-rules))
      (add-context "integers"
                   (contexts:make-builtin-context
                    '((use . "truth"))
                    builtins:integer-signature
                    builtins:merged-integer-rules))
      (add-context "rational-numbers"
                   (contexts:make-builtin-context
                    '((use . "truth"))
                    builtins:rational-signature
                    builtins:merged-rational-rules))
      (add-context "real-numbers"
                   (contexts:make-builtin-context
                    '((use . "truth"))
                    builtins:real-number-signature
                    builtins:merged-real-number-rules))
      (add-context "IEEE-floating-point"
                   (contexts:make-builtin-context
                    '((use . "integers"))
                    builtins:IEEE-float-signature
                    builtins:merged-IEEE-float-rules))))

;; An empty document has "builtins" in its library, ensuring that
;; it is always available.

(define empty-document
  (~> (document (hash) empty  (hash) (hash))
      (add-to-library "builtins" builtins #f)))
