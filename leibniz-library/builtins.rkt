#lang racket

(provide leibniz)

(require leibniz/builtin-contexts
         leibniz/documents
         threading)

; The builtins document
;
(define leibniz (~> empty-document
                    (add-builtin-context "truth" truth)
                    (add-builtin-context "integer" integers)
                    (add-builtin-context "rational-numbers" rational-numbers)
                    (add-builtin-context "real-numbers" real-numbers)
                    (add-builtin-context "IEEE-floating-point" IEEE-floating-point)
                    (add-builtin-context "IEEE-floating-point-with-conversion"
                                         IEEE-floating-point-with-conversion)))
