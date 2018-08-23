#lang pollen

◊(require leibniz/pollen)

◊define-meta[title]{A test document}
◊define-meta[author]{Konrad Hinsen}

◊b{Note:} There is nothing interesting in this document. It serves just for testing various features of the Pollen version of Leibniz.

◊+import[#:filename "/home/hinsen/projects/leibniz/pollen-examples/functions.html"]{functions}

◊+context{test}
◊+use{functions/ℝ→ℝ}

We define ◊+sort{foo} and ◊+sort{bar ⊆ foo}, and then ◊+op{a-foo : foo} and ◊+term{a-foo}
The only rule is:
    ◊+rule{a-foo ⇒ a-foo}

We also define ◊+op{f : ℝ→ℝ} and ◊+op{g : ℝ→ℝ} , just to use an imported op in
◊+term{f + g}.

Finally, a few useless assets:

◊+equation[#:label "eq1"]{a-foo = a-foo}

◊+term[#:label "term1"]{a-foo}

◊+rule[#:label "rule1"]{a-foo ⇒ a-foo}

◊+context{another-test}
◊+extend{test}

We define ◊+sort{baz} and ◊+sort{qoox}, and then ◊+var{ANY:baz}.
