#lang pollen

◊(require leibniz/pollen)

◊define-meta[title]{A document full of errors}
◊define-meta[author]{Konrad Hinsen}

◊+context{declaration-errors}

◊section{Semantic errors}

Cycle in sort graph: ◊+sort{foo ⊆ bar} with ◊+sort{bar ⊆ foo}
