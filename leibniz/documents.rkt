#lang racket

(provide empty-document
         add-to-library
         add-context
         lookup-context
         context-name-resolver
         read-xhtml-document
         library-path)

(require (prefix-in builtins: "./builtin-contexts.rkt")
         (prefix-in contexts: "./contexts.rkt")
         "./lightweight-class.rkt"
         threading
         xml
         txexpr
         file/sha1)

(module+ test
  (require rackunit))

;;
;; A document is a collection of contexts that can refer to each other by name.
;; Each document also keeps a library of other documents to whose contexts
;; its own contexts can refer. The dependency graph is acyclic by construction.
;;
(define-class document

  (field contexts order sha256 library library-by-name)
  ;; contexts: a hash mapping context names to contexts
  ;; order: a list of context names in the inverse order of definition
  ;; sha256: the sha256 hash of the HTML file from which the
  ;;         document was created, or #f for the document under
  ;;         construction from a Pollen source file
  ;; library: a hash mapping sha256 hashes to documents
  ;; library-by-name: a hash mapping document names to sha256 hashes

  ;; Set the sha256 hash for the document
  (define (set-sha256 sha256-from-file)
    (document contexts order sha256-from-file library library-by-name))

  ;; Add another document to the library.
  (define (add-to-library name library-document)
    (define library-document-sha256 (document-sha256 library-document))
    (define (hash-set* h k v)
      (if k
          (hash-set h k v)
          h))
    (document contexts order sha256
              (for/fold ([l (hash-set library
                                      library-document-sha256
                                      library-document)])
                        ([(lib-sha256 lib-doc) (document-library library-document)])
                (hash-set l lib-sha256 lib-doc))
              (hash-set library-by-name name library-document-sha256)))

  ;; Add a context defined by a context data structure.
  (define (add-context name cntxt)
    (document (hash-set contexts name cntxt)
              (cons name order)
              sha256
              library
              library-by-name))

  ;; Add a context defined by an xexpr
  (define (add-xexpr-context xexpr)
    (define cntxt (contexts:xexpr->context xexpr sha256))
    (define name (contexts:context-name cntxt))
    (define compiled (contexts:compile-context cntxt (context-name-resolver) #f))
    (add-context name compiled))

  (define (get-local-context name)
    (hash-ref contexts name))

  (define (lookup-context path)
    (define path-elements (map string-trim (string-split path "/")))
    (cond
      ;; local context
      [(= 1 (length path-elements))
       (define name (first path-elements))
       (unless (hash-has-key? contexts name)
         (error (format "no context named ~a" path)))
       (values sha256 name)]
      ;; context from a library document
      [(= 2 (length path-elements))
       (define doc-sha256 (hash-ref library-by-name (first path-elements) #f))
       (define name (second path-elements))
       (unless doc-sha256
         (error (format "no library document named ~a" (first path-elements))))
       (send (hash-ref library doc-sha256) lookup-context name)]
      [else
       (error (format "illegal context reference: ~a" path))]))

  (define (context-name-resolver)
    (λ (name doc-sha256)
      (define doc (if (and doc-sha256
                           (not (equal? doc-sha256 sha256)))
                      (hash-ref library doc-sha256)
                      this))
      (send doc get-local-context name))))

;; A document containing the builtin contexts

(define builtins
  (~> (document (hash) empty
                (~> #"builtins" sha256-bytes bytes->hex-string)
                (hash) (hash))
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
                    builtins:merged-IEEE-float-rules))
      (add-context "strings"
                   (contexts:make-builtin-context
                    empty
                    builtins:string-signature
                    builtins:string-rules))
      (add-context "contexts"
                   (contexts:make-builtin-context
                    '((use . "strings"))
                    builtins:context-signature
                    builtins:merged-context-rules))))

;; An empty document has "builtins" in its library, ensuring that
;; it is always available.

(define empty-document
  (~> (document (hash) empty #f (hash) (hash))
      (add-to-library "builtins" builtins)))

;; Read document from an HTML file

(define (read-xhtml filename)
  (~> filename
      open-input-file
      read-xml/element
      xml->xexpr))

(define ((has-tag? tag) element)
  (and (txexpr? element)
       (equal? tag (get-tag element))))

(define (xhtml->contexts xexpr)
  (~> xexpr
      (findf-txexpr
       (λ (x) (and ((has-tag? 'script) x)
                   (attrs-equal? '((id "leibniz-document") (type "application/xml"))
                                 (get-attrs x)))))
      (findf-txexpr (has-tag? 'leibniz-document))
      (findf*-txexpr (has-tag? 'context))))

(define (xhtml->document xexpr sha256-hex)
  (define document-element
    (~> xexpr
        (findf-txexpr
         (λ (x) (and ((has-tag? 'script) x)
                     (attrs-equal? '((id "leibniz-document") (type "application/xml"))
                                   (get-attrs x)))))
        (findf-txexpr (has-tag? 'leibniz-document))))
  (define docref-elements
    (~> document-element
        (findf-txexpr (has-tag? 'library))
        (findf*-txexpr (has-tag? 'document-ref))))
  (define context-elements
    (~> document-element
        (findf*-txexpr (has-tag? 'context))))
  (define document-with-imports
    (for/fold ([document (set-sha256 empty-document sha256-hex)])
              ([doc-ref (or docref-elements empty)])
      (define name (attr-ref doc-ref "id"))
      (define sha256-hex (attr-ref doc-ref "sha256"))
      (add-to-library document name
                      (read-library-document sha256-hex))))
  (for/fold ([document document-with-imports])
            ([xexpr context-elements])
    (add-xexpr-context document xexpr)))

(define (read-xhtml-document filename)
  (define sha256-hex (copy-to-library filename))
  (values (read-library-document sha256-hex) sha256-hex))

(define (read-library-document sha256-hex)
  (~> sha256-hex
      library-path
      read-xhtml
      (xhtml->document sha256-hex)))

;; Library management

(define library-directory
  (build-path (find-system-path 'home-dir) ".leibniz"))

(define (library-path sha256-hex)
  (build-path library-directory (string-append sha256-hex ".html")))

(define (copy-to-library filename)
  (define sha256-hex
    (~> filename
        (open-input-file #:mode 'binary)
        port->bytes
        sha256-bytes
        bytes->hex-string))
  (copy-file filename (library-path sha256-hex) #t)
  sha256-hex)
