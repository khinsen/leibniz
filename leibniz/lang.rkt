#lang racket

(provide empty-document
         (rename-out [cs:context context]
                     [cs:import import]
                     [cs:sort sort]
                     [cs:var var]
                     [cs:op op]
                     [cs:term term]
                     [cs:rule rule]
                     [cs:equation equation]
                     [cs:comment-sort comment-sort]
                     [cs:comment-op comment-op]
                     [cs:test test]
                     [cs:eval-term eval-term]
                     [cs:show-context show-context])
         inset
         xml signature-graphs
         use show reduce trace transform substitute
         (all-from-out scribble/base
                       scribble/doclang))

(require (except-in scribble/doclang sort)
         scribble/base
         (for-syntax syntax/parse)
         (prefix-in cs: "./context-syntax.rkt")
         "./documents.rkt"
         "./parser.rkt"
         "./formatting.rkt"
         (only-in megaparsack/text parse-string)
         (only-in megaparsack parse-result! parse-error->string)
         data/either
         threading
         (prefix-in operators: "./operators.rkt")
         (prefix-in terms: "./terms.rkt")
         (prefix-in equations: "./equations.rkt")
         (prefix-in rewrite: "./rewrite.rkt"))

;;
;; Support code for nicer formatting
;;
(define (inset . body)
  (apply nested
         (for/list ([element body])
           (if (equal? element "\n")
               (linebreak)
               element))
         #:style 'inset))

;;
;;; Generate XML output (not needed with the leibniz script)
;;
(define-syntax (xml stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ filename:str)
       #`(begin (write-xml #,leibniz-ref filename)
                (margin-note (hyperlink filename "XML")))])))

;;
;; Graphviz output (for debugging)
;;
(define-syntax (signature-graphs stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ directory:str)
       #`(write-signature-graphs #,leibniz-ref directory)])))

;;
;; Support for interactive exploration
;; REPL commands for use after loading a Leibniz module
;;
(define current-context-name (make-parameter #f))
(define current-context (make-parameter #f))
(define current-document (make-parameter #f))

(define-syntax (use stx)
  (let* ([leibniz-ref (datum->syntax stx 'leibniz)])
    (syntax-parse stx
      [(_ context-name:str)
       #`(begin
           (define context
             (with-handlers ([exn:fail? (λ (e) #f)])
               (get-context #,leibniz-ref context-name)))
           (unless context
             (error (format "Undefined context ~a" context-name)))
           (current-document #,leibniz-ref)
           (current-context-name context-name)
           (current-context context))])))

(define (assert-current-context)
  (unless (current-context-name)
    (error "No context has been selected")))

(define (parse-term term-str)
  (parse-string (to-eof/p term/p) term-str))

(define (parse-equation eq-str)
  (parse-string equation/p eq-str))

(define (parse-transformation tr-str)
  (parse-string transformation/p tr-str))

(define (make-parsed-term term-str)
  (match (parse-term term-str)
    [(success parsed-term)
     (make-term (current-document) (current-context-name) parsed-term #f)]
    [(failure message)
     #f]))

(define (make-parsed-term-or-eq str)
  (match (parse-equation str)
    [(success parsed-eq)
     (displayln parsed-eq)
     (make-equation (current-document) (current-context-name) parsed-eq #f)]
    [(failure message)
     (make-parsed-term str)]))

(define (make-parsed-transformation transformation-str)
  (make-transformation (current-document) (current-context-name)
                       (~> transformation-str
                           parse-transformation
                           parse-result!)
                       #f))

(define (display-term signature term)
  (when term
    (displayln (format "~a : ~a"
                       (terms:term.sort term)
                       (plain-text (format-term signature #f term))))))

(define (display-equation signature equation)
  (when equation
    (displayln (plain-text (format-equation #f equation signature)))))

(define (show term-str)
  (assert-current-context)
  (define signature (hash-ref (current-context) 'compiled-signature))
  (define term (make-parsed-term term-str))
  (display-term signature term))

(define (reduce term-str)
  (assert-current-context)
  (define signature (hash-ref (current-context) 'compiled-signature))
  (define rules (hash-ref (current-context) 'compiled-rules))
  (define term (make-parsed-term term-str))
  (define rterm (and term
                     (rewrite:reduce signature rules term)))
  (display-term signature rterm))

(define (trace term-str
               #:max-level [max-level 0]
               #:show-rules [show-rules #f])
  (assert-current-context)
  (define (display-trace level term rule rterm)
    (cond
      [(equal? level 0)
       (when show-rules
         (displayln (format "--- ~a"
                            (plain-text (format-rule #f rule signature)))))
       (displayln (format "... ~a"
                         (plain-text (format-term signature #f rterm))))]
      [(or (not max-level)
           (<= level max-level))
       (when show-rules
         (displayln (format "~a ~a"
                            (make-string (+ level 3) #\-)
                            (plain-text (format-rule #f rule signature)))))
       (displayln (format "~a ~a ⇒ ~a"
                          (make-string (+ level 3) #\+)
                          (plain-text (format-term signature #f term))
                          (plain-text (format-term signature #f rterm))))]))
  (define signature (hash-ref (current-context) 'compiled-signature))
  (define rules (hash-ref (current-context) 'compiled-rules))
  (define term (make-parsed-term term-str))
  (when term
    (rewrite:trace-reduce signature rules term display-trace))
  (void))

(define (transform term-or-eq-str transformation-str)
  (define signature (hash-ref (current-context) 'compiled-signature))
  (define sort-graph (operators:signature-sort-graph signature))
  (define term-or-eq (make-parsed-term-or-eq term-or-eq-str))
  (define transformation (make-parsed-transformation transformation-str))
  (cond
    [(terms:term? term-or-eq)
     (display-term signature
      (rewrite:transform signature transformation term-or-eq))]
    [(equations:equation? term-or-eq)
     (display-equation signature
      (rewrite:transform-equation signature transformation term-or-eq))]))

(define (substitute term-or-eq-str transformation-str)
  (define signature (hash-ref (current-context) 'compiled-signature))
  (define sort-graph (operators:signature-sort-graph signature))
  (define term-or-eq (make-parsed-term-or-eq term-or-eq-str))
  (define transformation (make-parsed-transformation transformation-str))
  (cond
    [(terms:term? term-or-eq)
     (display-term signature
      (rewrite:substitute signature transformation term-or-eq))]
    [(equations:equation? term-or-eq)
     (display-equation signature
      (rewrite:substitute-equation signature transformation term-or-eq))]))
