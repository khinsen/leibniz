#lang racket

(require "./context-syntax.rkt"
         "./builtin-contexts.rkt"
         (only-in "./rewrite.rkt" reduce))

(provide define-context with-context T RT eq =>
         reduce
         (all-from-out "./builtin-contexts.rkt"))
