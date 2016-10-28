#lang racket

(require leibniz
         leibniz/tools)

(define directory (vector-ref (current-command-line-arguments) 0))

(signature->graphviz (build-path directory "truth") truth)
(signature->graphviz (build-path directory "boolean") boolean)
(signature->graphviz (build-path directory "integers") integers)
(signature->graphviz (build-path directory "rational-numbers") rational-numbers)
(signature->graphviz (build-path directory "real-numbers") real-numbers)
(signature->graphviz (build-path directory "IEEE-floating-point") IEEE-floating-point)
