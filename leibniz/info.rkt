#lang info
(define collection "leibniz")
(define deps '("at-exp-lib"
               "base"
               "chk"
               "drracket"
               "functional-lib"
               "gui-lib"
               "megaparsack"
               "net-lib"
               "rackunit-lib"
               "scribble-lib"
               "sxml"
               "threading"))
(define build-deps '("racket-doc"
                     "scribble-doc"))
(define pkg-desc "Leibniz - A Digital Scientific Notation")
(define version "0.2")

(define racket-launcher-names '("leibniz"))
(define racket-launcher-libraries '("run.rkt"))

(define scribblings '(("leibniz.scrbl" (multi-page))))
