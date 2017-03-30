#lang racket/base

(require racket/cmdline
         (only-in scribble/render render)
         (only-in leibniz/documents write-xml))

(define (run)
  (command-line
   #:args (filename)
   (define doc (dynamic-require `(file ,filename) 'doc))
   (define leibniz (dynamic-require `(file ,filename) 'leibniz))
   (define-values (base name dir?) (split-path filename))
   (define xml-filename (path-replace-extension filename #".xml"))
   (render (list doc) (list name))
   (write-xml leibniz xml-filename)))

(module+ test)

(module+ main
  (run))
