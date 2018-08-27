#lang info
(define collection "leibniz")
(define deps '("base"
               "pollen"
               "functional-lib"
               "gui-lib"
               "megaparsack"
               "sha"
               "rackunit-lib"
               "chk"
               "net-lib"
               "threading"))
(define build-deps '("racket-doc"
                     "scribble-doc"))
(define pkg-desc "Leibniz - A Digital Scientific Notation")
(define version "0.3")

(define scribblings '(("leibniz.scrbl" (multi-page))))
