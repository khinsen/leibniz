#lang pollen

◊(require leibniz/pollen)

◊define-meta[title]{A document full of errors}
◊define-meta[author]{Konrad Hinsen}

◊+context{asset-errors}
◊section{Errors in assets}

◊+sort{foo} ◊+op{a-foo : foo}

Undefined op in term: ◊+term{an-undefined-op}

◊+context{declaration-errors}
◊section{Errors in context definition}

Cycle in sort graph: ◊+sort{foo ⊆ bar} with ◊+sort{bar ⊆ foo}
