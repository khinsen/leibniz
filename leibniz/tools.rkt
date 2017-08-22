#lang racket

(provide sort-graph->graphviz
         op->graphviz
         signature->graphviz)

(require "./sorts.rkt"
         "./operators.rkt"
         racket/function
         threading)

(define node-character-translation
  (hash "-" "DASH"))

(define (symbol->node symbol)
  (for/fold ([s (symbol->string symbol)])
            ([(from to) node-character-translation])
    (string-replace s from to)))

(define (sort-graph->graphviz s-graph [sort-or-kind #f])
  (define sorts
    (cond
      [(sort-graph? s-graph) s-graph]
      [(signature? s-graph) (signature-sort-graph s-graph)]
      [else (error "illegal input data")]))
  (define subset (if sort-or-kind
                     (kind sorts sort-or-kind)
                     (all-sorts sorts)))
  (printf "digraph sort_graph {\n")
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

(define (op->graphviz sig op-symbol arg-sorts)

  (define signature
    (cond
      [(signature? sig) sig]
      [else (error "illegal input data")]))
  (define sorts (signature-sort-graph signature))
  (define ranks (for/list ([rank (lookup-op-rank-list signature op-symbol arg-sorts)])
                  ; translate to the old return value of lookup-op-rank-list
                  (cons (first rank) (second rank))) )

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

  (define (indirect sa-graph)
    (for*/fold ([graph (hash)])
               ([(from tos) sa-graph]
                [to tos])
      (define second (hash-ref sa-graph to (set)))
      (hash-update graph from
                   (λ (ns) (set-union ns second))
                   second)))

  ; Simplify a subarity graph by removing all edges that
  ; are equivalent to existing indirect paths.
  (define (simplified sa-graph)
    (define i (indirect sa-graph))
    (for/hash ([(from tos) sa-graph])
      (values from (set-subtract tos (hash-ref i from)))))

  (printf "digraph op_ranks {\n")
  (printf "rankdir=BT;\n")
  (printf "node [shape=box];\n")
  (for ([rank ranks])
    (printf "~a [label=\"~a\"];\n"
            (node-label rank)
            (rank->string rank)))

  (for* ([(node1 node2-list) (simplified (subarity-graph ranks))]
         [node2 node2-list])
    (printf "~a -> ~a\n" node1 node2))

  (printf "label=\"Operator ~a for arity ~s\";\n"
          op-symbol
          arg-sorts)
  (printf "labelloc=top;\n")
  (printf "labeljust=left;\n")
  (printf "}\n"))

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
      [else (error "illegal input data")]))
  (define sorts (signature-sort-graph signature))
  (define base-path (expand-user-path directory-path))
  (make-directory* base-path)
  (with-output-to-file (build-path base-path "sorts.dot")
    (thunk (sort-graph->graphviz sorts))
    #:mode 'text #:exists 'truncate)
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
    (with-output-to-file (build-path base-path fname)
      (thunk (op->graphviz signature symbol arg-sorts))
      #:mode 'text #:exists 'truncate)))
