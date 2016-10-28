#!/bin/sh
racket draw-graphs.rkt ../graphs
find ../graphs -name \*.dot -exec dot -Tpdf -O {} \;
