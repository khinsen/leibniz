#lang leibniz

@title{Physical quantities}
@author{Konrad Hinsen}

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
  @inset{@rule{f1 × (f2 × q) ⇒ (f1 × f2) × q
               ∀ q:Q  ∀ f1:ℝ  ∀ f2:ℝ}
         @rule{f1 × ((f2 × q1) ÷ q2) ⇒ (f1 × f2) × (q1 ÷ q2)
               ∀ q1:Q  ∀ q2:Q  ∀ f1:ℝ  ∀ f2:ℝ}}

Replace division by multiplication:
  @inset{@rule{q ÷ f ⇒ (1 ÷ f) × q
               ∀ q:Q  ∀ f:ℝnz}
         @rule{q1 ÷ (f × q2) ⇒ (1 ÷ f) × (q1 ÷ q2)
               ∀ q1:Q  ∀ q2:Qnz  ∀ f:ℝnz}}

Remove quantities of zero magnitude from sums:
  @inset{@rule{q1 + (0 × q2) ⇒ q1
                ∀ q1:Q  ∀ q2:Q}
         @rule{q1 - (0 × q2) ⇒ q1
                ∀ q1:Q  ∀ q2:Q}
         @rule{(0 × q2) + q1 ⇒ q1
                ∀ q1:Q  ∀ q2:Q}
         @rule{(0 × q2) - q1 ⇒ q1
                ∀ q1:Q  ∀ q2:Q}}
}

@context["template" #:extend "quantities"]{

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
         @op{-(SQ) : SQ}
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
  @inset{@rule{(f1 × sq) + (f2 × sq) ⇒ (f1 + f2) × sq}
         @rule{(f1 × sq) - (f2 × sq) ⇒ (f1 - f2) × sq}}

Reduce quotients of two @sort{SQ}s to a number:
  @inset{@rule{sq1 ÷ (f × sq2) ⇒ (sq1 ÷ f) ÷ sq2}
         @rule{(f × sq1) ÷ sq2 ⇒ f × (sq1 ÷ sq2)}
         @rule{sq ÷ sq ⇒ 1}}
}

@context["template-test" #:extend "template"]{

@subsection{Tests}

Given two quantities @op{a : SQ} and @op{b : SQ} whose quotient we define
as @rule{b ÷ a ⇒ 10}, we can test the simplification rules:

  @inset{@test{2 × (3 × a) ⇒ 6 × a}
         @test{2 × (a ÷ 3) ⇒ 2/3 × a}
         @test{(2 × a) ÷ (3 × a) ⇒ 2/3}
         @test{(2 × b) ÷ (3 × a) ⇒ 20/3}
         @test{(2 × a) + (3 × a) ⇒ 5 × a}
         @test{(2 × a) - (3 × a) ⇒ -1 × a}}

}

@context["mass"
         #:insert-use ["template"
                       (rename-sort SQ M)
                       (rename-sort SQnz Mnz)]]{

@section{Mass}

Replace SQ by M and SQnz by Mnz in the template:

@show-context["mass"]
}


@context["length"
         #:insert-use ["template"
                       (rename-sort SQ L)
                       (rename-sort SQnz Lnz)]]{

@section{Length}

Replace SQ by L and SQnz by Lnz in the template:

@show-context["length"]
}


@context["time"
         #:insert-use ["template"
                       (rename-sort SQ T)
                       (rename-sort SQnz Tnz)]]{

@section{Time}

Replace SQ by T and SQnz by Tnz in the template (result now shown).

}

@context["velocity"
         #:insert-use ["template"
                       (rename-sort SQ V)
                       (rename-sort SQnz Vnz)]]{

@section{Velocity}

Replace SQ by V and SQnz by Vnz in the template (result not shown).

}

@context["acceleration"
         #:insert-use ["template"
                       (rename-sort SQ A)
                       (rename-sort SQnz Anz)]]{

@section{Acceleration}

Replace SQ by A and SQnz by Anz in the template (result not shown).

}

@context["force"
         #:insert-use ["template"
                       (rename-sort SQ F)
                       (rename-sort SQnz Fnz)]]{

@section{Force}

Replace SQ by F and SQnz by Fnz in the template (result not shown).

}

@context["angle"
         #:insert-use ["template"
                       (rename-sort SQ angle)
                       (rename-sort SQnz angle-nz)]]{

@sort{angle}
@op{π : angle}

}

@context["frequency"
         #:use "time"
         #:insert-use ["template"
                       (rename-sort SQ frequency)
                       (rename-sort SQnz frequency-nz)]]{

@op{frequency × T : ℝ}
@op{frequency-nz × Tnz : ℝnz}
@op{T × frequency : ℝ}
@op{Tnz × frequency-nz : ℝnz}
}

@context["angular-frequency"
         #:use "time"
         #:use "angle"
         #:insert-use ["template"
                       (rename-sort SQ angular-frequency)
                       (rename-sort SQnz angular-frequency-nz)]]{

@op{angular-frequency × T : angle}
@op{angular-frequency-nz × Tnz : angle-nz}
@op{T × angular-frequency : angle}
@op{Tnz × angular-frequency-nz : angle-nz}
}

@context["function-template" #:insert-use ["template"
                                           (rename-sort SQ SQD)
                                           (rename-sort SQnz SQDnz)]
                             #:insert-use ["template"
                                           (rename-sort SQ SQI)
                                           (rename-sort SQnz SQInz)]]{

@section{A template for functions from one quantity to another}

This template defines functions from a domain quantity @sort{SQD} to an
image quantity @sort{SQI}. The sort for such functions is @sort{SQD→SQI ⊆ Q→Q},
function application is defined by @op{SQD→SQI[SQD] : SQI}.

It is convenient to provide some arithmetic:
@itemlist[#:style 'ordered
  @item{Addition and subtraction of functions:
        @itemlist[
          @item{@op{f:SQD→SQI + g:SQD→SQI : SQD→SQI} with @linebreak[]
                @rule{(f + g)[x] ⇒ f[x] + g[x] ∀ x:SQD}}
          @item{@op{f:SQD→SQI - g:SQD→SQI : SQD→SQI} with @linebreak[]
                @rule{(f - g)[x] ⇒ f[x] - g[x] ∀ x:SQD}}
        ]}
  @item{Addition and subtraction of constants:
        @itemlist[
          @item{@op{f:SQD→SQI + q:SQI : SQD→SQI} with @linebreak[]
                @rule{(f + q)[x] ⇒ f[x] + q ∀ x:SQD}}
          @item{@op{f:SQD→SQI - q:SQI : SQD→SQI} with @linebreak[]
                @rule{(f - q)[x] ⇒ f[x] + q ∀ x:SQD}}
          @item{@op{q:SQI + f:SQD→SQI : SQD→SQI} with @linebreak[]
                @rule{(q + f)[x] ⇒ q + f[x] ∀ x:SQD}}
          @item{@op{q:SQI - f:SQD→SQI : SQD→SQI} with @linebreak[]
                @rule{(q - f)[x] ⇒ q - f[x] ∀ x:SQD}}
        ]}
  @item{Multiplication with scalars:
        @itemlist[
          @item{@op{s:ℝ × f:SQD→SQI : SQD→SQI} with @linebreak[]
                @rule{(s × f)[x] ⇒ s × f[x] ∀ x:SQD}}
          @item{@op{-(f:SQD→SQI) : SQD→SQI} with @linebreak[]
                @rule{-(f)[x] ⇒ -(f[x])  ∀ x:SQD}}]}
]
}

@context["function-with-derivative-template"
         #:insert-use ["function-template"]
         #:insert-use ["function-template"
                       (rename-sort SQI SQID)
                       (rename-sort SQInz SQIDnz)
                       (rename-sort SQD→SQI SQD→SQID)]]{

@section{A template for functions with derivatives}

The derivative of a function is given by @op{𝒟(SQD→SQI) : SQD→SQID},
where @sort{SQID ⊆ Qℝ} is the quotient of @sort{SQI} and @sort{SQD}.

It is a linear operator, i.e. for @var{f:SQD→SQI}, @var{g:SQD→SQI},
and @var{s:ℝ} we have
  @inset{@rule{𝒟(f + g) ⇒ 𝒟(f) + 𝒟(g)}
         @rule{𝒟(f - g) ⇒ 𝒟(f) - 𝒟(g)}
         @rule{𝒟(s × f) ⇒ s × 𝒟(f)}}

}

@context["function-with-finite-difference-template"
         #:insert-extend["function-with-derivative-template"]]{

In numerical approximations, the derivative operator
@op{𝒟(SQD→SQI) : SQD→SQID} is replaced by the finite-difference
operator @op{Δ(f:SQD→SQI, h:SQDnz) : SQD→SQID}. A finite-difference
approximation is characterized by a parameter @var{h:SQDnz}, assumed
to be a sufficiently small quantity.

Like the derivative operators, the finite-difference operator is linear, i.e.
for two functions @var{f:SQD→SQI} and @var{g:SQD→SQI}, and a
numerical scaling factor @var{s:ℝ}, we have
  @inset{@rule{Δ(f + g, h) ⇒ Δ(f, h) + Δ(g, h)}
         @rule{Δ(f - g, h) ⇒ Δ(f, h) - Δ(g, h)}
         @rule{Δ(s × f, h) ⇒ s × Δ(f, h)}}

}


@;signature-graphs["quantities.sig"]

