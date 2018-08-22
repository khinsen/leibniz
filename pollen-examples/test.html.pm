#lang pollen

◊(require leibniz/pollen)

◊define-meta[title]{A test document}
◊define-meta[author]{Konrad Hinsen}

◊+context{test}

We define ◊+sort{foo} and ◊+sort{bar ⊆ foo}, and then ◊+op{a-foo : foo} and ◊+term{a-foo}
The only rule is:
    ◊+rule{a-foo ⇒ a-foo}

◊+equation[#:label "eq1"]{a-foo = a-foo}

◊+term[#:label "term1"]{a-foo}

◊+rule[#:label "rule1"]{a-foo ⇒ a-foo}

◊+context{another-test}
◊+extend{test}

We define ◊+sort{baz} and ◊+sort{qoox}, and then ◊+var{ANY:baz}.
