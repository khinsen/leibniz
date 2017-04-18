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

Function composition is defined by
  @inset{@op{f:ℝ→ℝ ○ g:ℝ→ℝ : ℝ→ℝ} with
         @rule{(f ○ g)[x] ⇒ f[g[x]] ∀ x:ℝ}}
}

@context["derivatives-ℝ→ℝ"
         #:extend "ℝ→ℝ"]{

@section{Derivatives}

The derivative of a function is given by @op{𝒟(ℝ→ℝ) : ℝ→ℝ}, which is
a linear operator:
  @inset{@rule{𝒟(f + g) ⇒ 𝒟(f) + 𝒟(g)}
         @rule{𝒟(f - g) ⇒ 𝒟(f) - 𝒟(g)}
         @rule{𝒟(s × f) ⇒ s × 𝒟(f)}}

The derivatives of products and compositions of two functions are given by:
  @inset{@rule{𝒟(f × g) ⇒ (𝒟(f) × g) + (f × 𝒟(g))}
         @rule{𝒟(f ○ g) ⇒ (𝒟(f) ○ g) × 𝒟(g)}}

}

@context["finite-differences-ℝ→ℝ"
         #:extend "ℝ→ℝ"]{

@section{Finite difference operators}

In numerical calculations, derivatives must often be approximated by finite
differences. Since there are many possible schemes for computing finite differences,
we define multiple @sort{finite-difference-operator}s with
@op{finite-difference-operator[fn:ℝ→ℝ, h:ℝ] : ℝ→ℝ}, where @term{h} is the step size.

Next, we define @op{Δ : finite-difference-family} and @sort{finite-difference-scheme}
such that @op{finite-difference-family_{finite-difference-scheme} : finite-difference-operator}.

With these definitions, the three most common finite-difference schemes are:
@itemlist[
    @item{@op{forward : finite-difference-scheme}: @linebreak[]
          @rule{Δ_{forward}[fn, h][x] ⇒ (fn[x + h] - fn[x]) ÷ h  ∀ x:ℝ}}
    @item{@op{backward : finite-difference-scheme}: @linebreak[]
          @rule{Δ_{backward}[fn, h][x] ⇒ (fn[x] - fn[x - h]) ÷ h  ∀ x:ℝ}}
    @item{@op{central : finite-difference-scheme}: @linebreak[]
          @rule{Δ_{central}[fn, h][x] ⇒ (fn[x + (h ÷ 2)] - fn[x - (h ÷ 2)]) ÷ h  ∀ x:ℝ}}]
}