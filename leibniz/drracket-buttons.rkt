#lang racket/base

; This code is taken from the Scribble pluging for DrRacket with minor
; modifications. I don't understand what all of this does in detail, so
; this is probably not optimal.

(require racket/runtime-path
         racket/gui/base
         racket/class
         mrlib/bitmap-label
         racket/system
         net/sendurl
         drracket/tool-lib)

(provide drracket-buttons)

(module test racket/base)

(define-runtime-path leibniz-png-path "leibniz-button.png")
(define leibniz.png (make-object bitmap% leibniz-png-path 'png/mask))

(define-namespace-anchor anchor)

(define original-error-display-handler (error-display-handler))

(define (make-render-button label bmp number)
  (list 
   label
   bmp
   (λ (drs-frame)
     (define fn (send (send drs-frame get-definitions-text) get-filename))
     (define html-fn (path-replace-suffix fn #".html"))
     (define xml-fn (path-replace-suffix fn #".xml"))
     (cond
       [fn
        (parameterize ([drracket:rep:after-expression
                        (λ ()
                          (define scribble-doc
                            (with-handlers ((exn:fail? (λ (x) #f))) (eval 'doc)))
                          (define leibniz-doc
                            (with-handlers ((exn:fail? (λ (x) #f))) (eval 'leibniz)))
                          ;; if (eval 'doc) goes wrong, then we assume that's because of
                          ;; an earlier failure, so we just don't do anything.
                          (when scribble-doc
                            (printf "leibniz: loading xref\n")
                            (define xref ((dynamic-require 'setup/xref 'load-collections-xref)))
                            (printf "leibniz: rendering\n")
                            (parameterize ([current-input-port (open-input-string "")])
                              ((dynamic-require 'scribble/render 'render) 
                               (list scribble-doc)
                               (list fn)
                               #:render-mixin (dynamic-require 'scribble/html-render
                                                               'render-mixin)
                               #:xrefs (list xref)))
                            (send-url/file html-fn))
                          (when leibniz-doc
                            (printf "leibniz: xml generation\n")
                            ((dynamic-require 'leibniz/documents 'write-xml)
                             leibniz-doc xml-fn)))]) 
          (send drs-frame execute-callback))]
       [else
        (message-box "Leibniz" "Cannot render buffer without filename")]))
   number))

(define drracket-buttons
  (list (make-render-button "Leibniz" leibniz.png 99)))
