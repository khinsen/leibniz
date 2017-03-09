#lang racket

(provide leibniz)

(require leibniz/builtin-contexts
         leibniz/documents
         threading)

; The builtins document
;
(define leibniz (~> empty-document
                    (add-context "truth"
                                 empty
                                 truth)
                    (add-context "integers"
                                 (list (cons "truth" #f))
                                 integers)
                    (add-context "rational-numbers"
                                 (list (cons "truth" #f))
                                 rational-numbers)
                    (add-context "real-numbers"
                                 (list (cons "truth" #f))
                                 real-numbers)
                    (add-context "IEEE-floating-point"
                                 (list (cons "integers" #f))
                                 IEEE-floating-point)
                    (add-context "IEEE-floating-point-with-conversion"
                                 (list (cons "IEEE-floating-point" #f)
                                       (cons "rational-numbers" #f))
                                 IEEE-floating-point-with-conversion)))
