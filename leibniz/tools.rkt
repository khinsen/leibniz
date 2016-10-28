#lang racket

(provide sort-graph->graphviz
         op->graphviz
         signature->graphviz)

(require "./sorts.rkt"
         "./operators.rkt"
         "./contexts.rkt"
         racket/function
         threading)

(define node-character-translation
  (hash "-" "DASH"))

(define (symbol->node symbol)
  (for/fold ([s (symbol->string symbol)])
            ([(from to) node-character-translation])
    (string-replace s from to)))

(define (sort-graph->graphviz file-path s-graph [sort-or-kind #f])
  (define sorts
    (cond
      [(sort-graph? s-graph) s-graph]
      [(signature? s-graph) (signature-sort-graph s-graph)]
      [(context? s-graph) (signature-sort-graph (context-signature s-graph))]
      [else (error "illegal input data")]))
  (define subset (if sort-or-kind
                     (kind sorts sort-or-kind)
                     (all-sorts sorts)))
  (with-output-to-file (expand-user-path file-path)
    (thunk (printf "digraph sort_graph {\n")
           (for ([sort subset])
             (printf "~a [label=\"~a\"];\n"
                     (symbol->node sort) (symbol->string sort)))
           (printf "rankdir=BT;\n")
           (for ([ss (all-subsort-relations sorts)]
                 #:when (set-member? subset (car ss)))
             (printf "~a->~a;\n"
                     (symbol->node (car ss))
                     (symbol->node (cdr ss))))
           (when sort-or-kind
             (printf "label=\"Sort graph of ~a\";\n"
                     (constraint->string sorts subset))
             (printf "labelloc=top;\n")
             (printf "labeljust=left;\n"))
           (printf "}\n"))
    #:mode 'text #:exists 'truncate))

(define (op->graphviz file-path sig op-symbol arg-sorts)

  (define signature
    (cond
      [(signature? sig) sig]
      [(context? sig) (context-signature sig)]
      [else (error "illegal input data")]))
  (define sorts (signature-sort-graph signature))
  (define ranks (lookup-op-rank-list signature op-symbol arg-sorts))

  (define (node-label rank)
    (apply string-append (map symbol->node (cons (cdr rank) (car rank)))))

  (define (rank->string rank)
    (format "~a\n-> ~a" (car rank) (cdr rank)))

  (define (is-subarity? arity1 arity2)
    (for/and ([s1 arity1] [s2 arity2])
      (conforms-to? sorts s1 s2)))

  (define (subarity-graph ranks)
    (for*/fold ([graph (hash)])
               ([rank1 ranks]
                [rank2 ranks]
                #:when (and (not (equal? rank1 rank2))
                            (is-subarity? (car rank1) (car rank2))))
      (hash-update graph (node-label rank1)
                   (λ (ns) (set-add ns (node-label rank2)))
                   (set (node-label rank2)))))

  (with-output-to-file (expand-user-path file-path)
    (thunk (printf "digraph op_ranks {\n")
           (printf "rankdir=BT;\n")
           (printf "node [shape=box];\n")
           (for ([rank ranks])
             (printf "~a [label=\"~a\"];\n"
                     (node-label rank)
                     (rank->string rank)))
           (for* ([(node1 node2-list) (subarity-graph ranks)]
                  [node2 node2-list])
             (printf "~a -> ~a\n" node1 node2))
           (printf "label=\"Operator ~a for arity ~s\";\n"
                   op-symbol
                   arg-sorts)
           (printf "labelloc=top;\n")
           (printf "labeljust=left;\n")
           (printf "}\n"))
    #:mode 'text #:exists 'truncate))

(define filename-character-translation
  (hash "/" "SLASH"
        "*" "STAR"
        "=" "EQUAL"
        ">" "GREATER"
        "<" "LESS"
        "^" "HAT"))

(define (symbol->path-element symbol)
  (for/fold ([s (symbol->string symbol)])
            ([(from to) filename-character-translation])
    (string-replace s from to)))

(define (signature->graphviz directory-path sig)
  (define signature
    (cond
      [(signature? sig) sig]
      [(context? sig) (context-signature sig)]
      [else (error "illegal input data")]))
  (define sorts (signature-sort-graph signature))
  (define base-path (expand-user-path directory-path))
  (make-directory* base-path)
  (sort-graph->graphviz (build-path base-path "sorts.dot") sorts)
  (for ([(symbol k-arity) (ops-by-kind-arity signature)])
    (define arg-sorts (for/list ([kind k-arity])
                        (set-first (maximal-sorts sorts kind))))
    (define fname
      (string-append
       (string-join (map symbol->path-element
                         (cons 'op
                               (cons symbol arg-sorts)))
                    "-")
       ".dot"))
    (op->graphviz (build-path base-path fname)
                  signature symbol arg-sorts)))
