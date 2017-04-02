#lang s-exp syntax/module-reader

leibniz/lang

#:read read-inside
#:read-syntax read-syntax-inside
#:whole-body-readers? #t
#:wrapper1 (lambda (t) (list* 'doc 'values '() '(define leibniz empty-document) '(provide leibniz) (t)))
#:language-info (scribble-base-language-info)
#:info (leibniz-info)

(require scribble/reader
         (only-in scribble/base/reader
                  scribble-base-info
                  scribble-base-language-info))

(define (leibniz-info)
  (lambda (key defval default)
    (case key
      [(drracket:toolbar-buttons)
       (dynamic-require 'leibniz/drracket-buttons 'drracket-buttons)
       ; (list (make-render-button "Scribble HTML" html.png "--html" #".html" 99))
       ]
      [else ((scribble-base-info) key defval default)])))

