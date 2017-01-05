#lang racket

(require "./rewrite-syntax.rkt"
         "./builtin-contexts.rkt"
         (only-in "./rewrite.rkt" reduce)
         "./numeric.rkt")

(provide define-context context with-context T RT R A S eq tr
         reduce
         real->IEEE-binary32 real->IEEE-binary64
         (all-from-out "./builtin-contexts.rkt"))
