#lang pollen

◊(require leibniz/pollen)

◊define-meta[title]{A test document}
◊define-meta[author]{Konrad Hinsen}

◊+context{test}

We define ◊+sort{foo} and ◊+sort{bar ⊆ foo}, and then ◊+op{a-foo : foo}.

◊b{◊+context{foo}}

◊+context{another-test}
◊+extend{test}

We define ◊+sort{baz} and ◊+sort{qoox}.

◊b{◊+context{bar}}
