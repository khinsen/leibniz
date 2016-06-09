#lang racket

; http://jeapostrophe.github.io/2013-11-12-condd-post.html

(provide condd)

(require (for-syntax syntax/parse))

(define-syntax (condd stx)
  (syntax-parse stx
    [(_)
     #'(error 'condd "Fell through without else clause")]
    [(_ [else . e])
     #'(let () . e)]
    [(_ #:do d . tail)
     #'(let () d (condd . tail))]
    [(_ [t:expr . e] . tail)
     #'(if t
         (let () . e)
         (condd . tail))]))
