#lang leibniz

@title{Functions}
@author{Konrad Hinsen}

@context["ℝ→ℝ"
         #:use "builtins/real-numbers"]{

@section{Real functions of one variable}

The sort @sort{ℝ→ℝ} describes real functions of one real variable.
Function application is defined by @op{ℝ→ℝ[ℝ] : ℝ}. Note that this
implies that the domain of the function is the full set of real
numbers, which excludes functions with singularities as well as
partial functions.

It is convenient to provide basic arithmetic on functions:
@itemlist[
  @item{@op{f:ℝ→ℝ + g:ℝ→ℝ : ℝ→ℝ} with @linebreak[]
        @rule{(f + g)[x] ⇒ f[x] + g[x] ∀ x:ℝ}}
  @item{@op{f:ℝ→ℝ - g:ℝ→ℝ : ℝ→ℝ} with @linebreak[]
        @rule{(f - g)[x] ⇒ f[x] - g[x] ∀ x:ℝ}}
  @item{@op{f:ℝ→ℝ × g:ℝ→ℝ : ℝ→ℝ} with @linebreak[]
        @rule{(f × g)[x] ⇒ f[x] × g[x] ∀ x:ℝ}}
  @item{@op{s:ℝ × g:ℝ→ℝ : ℝ→ℝ} with @linebreak[]
        @rule{(s × g)[x] ⇒ s × g[x] ∀ x:ℝ}}]

We do not define division as this requires more elaborate definitions
to handle the case of functions with zeros.

The derivative of a function is given by @op{𝒟(ℝ→ℝ) : ℝ→ℝ}, which is
a linear operator:
  @inset{@rule{𝒟(f + g) ⇒ 𝒟(f) + 𝒟(g)}}

}
