#lang info
(define collection "leibniz")
(define deps '("base"
               "threading"
               "sha"
               "pollen"
               "functional-lib"
               "megaparsack"
               "rackunit-lib"
               "chk"
               "net-lib"))
(define build-deps '("racket-doc"
                     "scribble-doc"))
(define pkg-desc "Leibniz - A Digital Scientific Notation")
(define version "0.3")

(define scribblings '(("leibniz.scrbl" (multi-page))))
