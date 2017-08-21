#lang racket

(provide
 (contract-out
  [reduce (context? term? . -> . term?)]
  [reduce-equation (context? equation? . -> . equation?)]
  [transform (context? transformation? term? . -> . term?)]
  [transform-equation (context? transformation? equation?
                       . -> . equation?)]
  [substitute (context? transformation? term? . -> . term?)]
  [substitute-equation (context? transformation? equation?
                        . -> . equation?)]))

(require (prefix-in rewrite: "./rewrite.rkt")
         "./contexts.rkt"
         "./equations.rkt"
         "./terms.rkt")

(define (reduce context term)
  (rewrite:reduce (context-signature context) (context-rules context) term))

(define (reduce-equation context equation)
  (rewrite:reduce-equation (context-signature context) (context-rules context)
                           equation))

(define (transform context transformation term)
  (rewrite:transform (context-signature context)
                     transformation term))

(define (transform-equation context transformation equation)
  (rewrite:transform-equation (context-signature context)
                              transformation equation))

(define (substitute context transformation term)
  (rewrite:substitute (context-signature context)
                     transformation term))

(define (substitute-equation context transformation equation)
  (rewrite:substitute-equation (context-signature context)
                               transformation equation))
