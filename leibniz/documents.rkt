#lang racket

(provide empty-document
         add-to-library
         add-context
         get-context
         context-name-resolver
         (rename-out [document-dependency-paths include-prefixes])
         read-xhtml-document
         library-path)

(require (prefix-in builtins: "./builtin-contexts.rkt")
         (prefix-in contexts: "./contexts.rkt")
         "./lightweight-class.rkt"
         threading
         xml
         txexpr
         sha)

(module+ test
  (require rackunit))

;;
;; A document is a collection of contexts that can refer to each other by name.
;; Each document also keeps a library of other documents to whose contexts
;; its own contexts can refer. The dependency graph is acyclic by construction.
;;
(define-class document

  (field contexts order sha256 library library-refs dependency-paths)
  ;; contexts: a hash mapping context names to hashes with keys
  ;;           'includes 'sorts 'ops 'vars 'rules 'assets,
  ;;           values are lists/sets/hashes of the declarations in
  ;;            each category
  ;; order: a list of context names in the inverse order of definition
  ;; sha256: the sha256 hash of the HTML file from which the
  ;;         document was created, or #f if the document was created
  ;;         by other means
  ;; library: a hash mapping document names to documents
  ;; library-refs: a hash mapping document names to external references
  ;;               (filenames for now, later maybe DOIs or hashes)
  ;; dependency-paths: a hash mapping sha256 hashes to include prefixes

  ;; Set the sha256 hash for the document
  (define (set-sha256 sha256-from-file)
    (document contexts order sha256-from-file
              library library-refs dependency-paths))

  ;; Add another document to the library.
  (define (add-to-library name library-document external-ref)
    (define (hash-set* h k v)
      (if k
          (hash-set h k v)
          h))
    (document contexts order sha256
              (hash-set library name library-document)
              (hash-set library-refs name external-ref)
              (for/fold ([dp (hash-set* dependency-paths
                                        (document-sha256 library-document)
                                        name)])
                        ([(lib-sha256 include-prefix)
                          (document-dependency-paths library-document)])
                (hash-set* dp
                           lib-sha256
                           (string-append name "/" include-prefix)))))

  ;; Add a context defined by a context data structure.
  (define (add-context name cntxt)
    (document (hash-set contexts name cntxt)
              (cons name order)
              sha256
              library
              library-refs
              dependency-paths))

  ;; Add a context defined by an xexpr
  (define (add-xexpr-context xexpr)
    (define-values (cntxt name) (contexts:xexpr->context+name xexpr sha256))
    (add-context name (contexts:compile-context cntxt (context-name-resolver))))

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
  (~> (document (hash) empty #f (hash) (hash) (hash))
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
  (~> (document (hash) empty #f (hash) (hash) (hash))
      (add-to-library "builtins" builtins #f)))

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
                      (read-xhtml-document (library-path sha256-hex))
                      sha256-hex)))
  (for/fold ([document document-with-imports])
            ([xexpr context-elements])
    (add-xexpr-context document xexpr)))

(define (read-xhtml-document filename)
  (define sha256-hex (copy-to-library filename))
  (define document
    (~> filename
        read-xhtml
        (xhtml->document sha256-hex)))
  (values document sha256-hex))

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
        sha256
        bytes->hex-string))
  (copy-file filename (library-path sha256-hex) #t)
  sha256-hex)
