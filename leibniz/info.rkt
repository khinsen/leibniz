#lang info
(define collection "leibniz")
(define deps '("base"
               "threading"
               "rackunit-lib"
               "chk"
               "megaparsack"
               "sxml"))
(define pkg-desc "Leibniz - A Digital Scientific Notation")
(define version "0.2")

(define racket-launcher-names '("leibniz"))
(define racket-launcher-libraries '("run.rkt"))

(define scribblings '(("leibniz.scrbl" (multi-page))))
