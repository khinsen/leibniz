#lang racket

(provide leibniz)

(require leibniz/builtin-contexts
         leibniz/documents
         threading)

; The builtins document
;
(define leibniz (~> empty-document
                    (add-context "truth" truth)
                    (add-context "integer" integers)
                    (add-context "rational-numbers" rational-numbers)
                    (add-context "real-numbers" real-numbers)
                    (add-context "IEEE-floating-point" IEEE-floating-point)
                    (add-context "IEEE-floating-point-with-conversion"
                                 IEEE-floating-point-with-conversion)))
