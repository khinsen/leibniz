#lang leibniz

@title{Physical quantities}
@author{Konrad Hinsen}

@import["builtins" leibniz-library/builtins]

@context["quantities" #:use "builtins/real-numbers"]{

@section{Generic quantities}

We define @sort{Q} to represent any physical quantity, and @sort{Qnz ⊆ Q} to represent
the subset of non-zero quantities by which it is admissible to divide. The product and
quotient of any two quantities is then again a quantity, with appropriate special cases
for quantities that can be proven to be non-zero:
  @inset{@op{Q × Q : Qℝ}
         @op{Qnz × Qnz : Qℝnz}
         @op{Q ÷ Qnz : Qℝ}
         @op{Qnz ÷ Qnz : Qℝnz}}

The result sort of these operators is not @sort{Q} but @sort{Qℝ} or @sort{Qℝnz ⊆ Qℝ},
because in the special case of a quotient of same-kind quantities, the result is a
pure number. We therefore define
  @inset{@sort{Q ⊆ Qℝ}
         @sort{Qnz ⊆ Qℝnz}
         @sort{ℝ ⊆ Qℝ}
         @sort{ℝnz ⊆ Qℝnz}}

We can also multiply or divide quantities by numbers:
  @inset{@op{ℝ × Q : Q}
         @op{ℝnz × Qnz : Qnz}
         @op{Q ÷ ℝnz : Q}
         @op{Qnz ÷ ℝnz : Qnz}}

The simplification strategy is to reduce quantities to the form
f × q, with q a non-reducible quantity, wherever possible.

Combine multiple numerical prefactors into one:
  @inset{@rule{f1 × f2 × q ⇒ (f1 × f2) × q
               ∀ q:Q  ∀ f1:ℝ  ∀ f2:ℝ}
         @rule{f1 × (f2 × q1) ÷ q2 ⇒ (f1 × f2) × q1 ÷ q2
               ∀ q1:Q  ∀ q2:Q  ∀ f1:ℝ  ∀ f2:ℝ}}

Replace division by multiplication:
  @inset{@rule{q ÷ f ⇒ (1 ÷ f) × q
               ∀ q:Q  ∀ f:ℝnz}
         @rule{q1 ÷ f × q2 ⇒ (1 ÷ f) × q1 ÷ q2
               ∀ q1:Q  ∀ q2:Qnz  ∀ f:ℝnz}}

}

@context["template" #:use "quantities"]{

@section{A template for specific quantities}

The definitions and rules for specific quantities such as mass or time
are essentially the same. We define a template for a fictitious quantity
@sort{SQ ⊆ Q} with @sort{SQnz ⊆ Qnz} and @sort{SQnz ⊆ SQ}, and
derive the real physical quantities by name substitution.

The sum and difference of two same-kind quantities is again a quantity
of the same kind:
  @inset{@op{SQ + SQ : SQ}
         @op{SQ - SQ : SQ}}

Multiplication and division by numbers also yields same-kind quantities:
  @inset{@op{ℝ × SQ : SQ}
         @op{ℝnz × SQnz : SQnz}
         @op{SQ ÷ SQnz : ℝ}
         @op{SQnz ÷ SQnz : ℝnz}
         @op{SQ ÷ ℝnz : SQ}
         @op{SQnz ÷ ℝnz : SQnz}}

Finally, same-kind quantities can be compared:
  @inset{@op{SQ < SQ : boolean}
         @op{SQ > SQ : boolean}
         @op{SQ ≤ SQ : boolean}
         @op{SQ ≥ SQ : boolean}}

In the simplification rules, we use the variables @var{sq:SQ}, @var{sq1:SQ},
@var{sq2:SQ} and @var{f:ℝ}, @var{f1:ℝ}, @var{f2:ℝ}.

Combine sums and differences of the same @sort{SQ} with different numerical prefactors:
  @inset{@rule{(f1 × sq) + f2 × sq ⇒ (f1 + f2) × sq}
         @rule{(f1 × sq) - f2 × sq ⇒ (f1 - f2) × sq}}

Reduce quotients of two @sort{SQ}s to a number:
  @inset{@rule{sq1 ÷ f × sq2 ⇒ (sq1 ÷ f) ÷ sq2}
         @rule{(f × sq1) ÷ sq2 ⇒ f × sq1 ÷ sq2}
         @rule{sq ÷ sq ⇒ 1}}
}

@context["template-test" #:use "template"]{

@subsection{Tests}

Given two quantities @op{a : SQ} and @op{b : SQ} whose quotient we define
as @rule{b ÷ a ⇒ 10}, we can test the simplification rules:

  @inset{@test{2 × 3 × a ⇒ 6 × a}
         @test{2 × a ÷ 3 ⇒ 2/3 × a}
         @test{(2 × a) ÷ 3 × a ⇒ 2/3}
         @test{(2 × b) ÷ 3 × a ⇒ 20/3}
         @test{(2 × a) + 3 × a ⇒ 5 × a}
         @test{(2 × a) - 3 × a ⇒ -1 × a}}

}

@context["mass"
         #:use "quantities"
         #:transform-context "template"
                               [hide-vars
                               (rename-sort SQ M)
                               (rename-sort SQnz Mnz)]]{

@section{Mass}

Replace SQ by M and SQnz by Mnz in the template:

@show-context["mass"]
}


@context["length"
         #:use "quantities"
         #:transform-context "template"
                              [hide-vars
                               (rename-sort SQ L)
                               (rename-sort SQnz Lnz)]]{

@section{Length}

Replace SQ by L and SQnz by Lnz in the template:

@show-context["length"]
}


@context["time"
         #:use "quantities"
         #:transform-context "template"
                              [hide-vars
                               (rename-sort SQ T)
                               (rename-sort SQnz Tnz)]]{

@section{Time}

Replace SQ by T and SQnz by Tnz in the template (result now shown).

}

@context["velocity"
         #:use "quantities"
         #:transform-context "template"
                              [hide-vars
                               (rename-sort SQ V)
                               (rename-sort SQnz Vnz)]]{

@section{Velocity}

Replace SQ by V and SQnz by Vnz in the template (result not shown).

}

@context["acceleration"
         #:use "quantities"
         #:transform-context "template"
                              [hide-vars
                               (rename-sort SQ A)
                               (rename-sort SQnz Anz)]]{

@section{Acceleration}

Replace SQ by A and SQnz by Anz in the template (result not shown).

}

@context["kinematics" #:use "length"   #:use "time"
                      #:use "velocity" #:use "acceleration"]{

@section{Kinematics}

The derived quantities @sort{V} and @sort{A} are obtained as quotients
of the fundamental quantities @sort{L} and @sort{T}.

Velocities are obtained by dividing a length by a time:
  @inset{@op{L ÷ Tnz : V}
        @op{Lnz ÷ Tnz : Vnz}}

Accelerations are the result of dividing a velocity by a time:
  @inset{@op{V ÷ Tnz : A}
        @op{Vnz ÷ Tnz : Anz}}

}

@context["kinematics-example" #:use "kinematics"]{

@subsection{Example}

We consider a point that moves on a straight line starting at time 0 from the origin.
At time @op{t1 : Tnz} it has distance @op{d1 : L} from the origin, at
time @op{t2 : Tnz}, @term{t2 > t1} the distance is @op{d2 : L}.

The average velocity from time 0 to @term{t1} is then
  @inset{@op{v1 : V}
         @rule{v1 ⇒ d1 ÷ t1}.}
and the average velocity between 0 and @term{t2} is
  @inset{@op{v2 : V}
         @rule{v2 ⇒ d2 ÷ t2}.}
The average acceleration is given by
  @inset{@op{a : A}
         @rule{a ⇒ 2 × (v2 - v1) ÷ t2 - t1}
         @eval-term{a}.}

}

@context["kinematics-nummerical-example" #:use "kinematics-example"]{

Introducing a time unit @op{s : Tnz} and a length unit @op{m : Lnz}, we
can assign numerical values:
  @inset{@rule{t1 ⇒ 3 × s},  @rule{d1 ⇒ 20 × m}
         @rule{t2 ⇒ 6 × s},  @rule{d2 ⇒ 50 × m}
         @eval-term{v1}
         @eval-term{v2}
         @eval-term{a}.}

}

@xml["quantities.xml"]
@signature-graphs["quantities.sig"]

