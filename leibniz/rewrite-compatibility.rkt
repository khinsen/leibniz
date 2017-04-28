#lang racket

(provide
 (contract-out
  [reduce (context? term? . -> . term?)]
  [reduce-equation ((context? equation?) ((or/c #f symbol?))
                    . ->* . equation?)]
  [transform (context? transformation? term? . -> . term?)]
  [transform-equation ((context? transformation? equation?) ((or/c #f symbol?))
                       . ->* . equation?)]
  [substitute (context? transformation? term? . -> . term?)]
  [substitute-equation ((context? transformation? equation?) ((or/c #f symbol?))
                        . ->* . equation?)]))

(require (prefix-in rewrite: "./rewrite.rkt")
         "./contexts.rkt"
         "./equations.rkt"
         "./terms.rkt")

(define (reduce context term)
  (rewrite:reduce (context-signature context) (context-rules context) term))

(define (reduce-equation context equation [new-label #f])
  (rewrite:reduce-equation (context-signature context) (context-rules context)
                           equation new-label))

(define (transform context transformation term)
  (rewrite:transform (context-signature context)
                     transformation term))

(define (transform-equation context transformation equation [new-label #f])
  (rewrite:transform-equation (context-signature context)
                              transformation equation new-label))

(define (substitute context transformation term)
  (rewrite:substitute (context-signature context)
                     transformation term))

(define (substitute-equation context transformation equation [new-label #f])
  (rewrite:substitute-equation (context-signature context)
                               transformation equation new-label))
