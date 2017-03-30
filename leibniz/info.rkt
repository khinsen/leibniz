#lang info
(define collection "leibniz")
(define deps '("base"
               "threading"
               "rackunit-lib"
               "chk"
               "sweet-exp-lib"
               "sxml"))
(define pkg-desc "Leibniz - A Digital Scientific Notation")
(define version "0.1")
(define compile-omit-paths 'all)

(define racket-launcher-names '("leibniz"))
(define racket-launcher-libraries '("run.rkt"))
