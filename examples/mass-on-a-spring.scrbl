#lang leibniz

@import["mechanics" "mechanics.xml"]
@import["quantities" "quantities.xml"]

@title{Motion of a mass on a spring}
@author{Konrad Hinsen}

@context["mass-on-a-spring"
         #:use "mechanics/dynamics"
         #:use "quantities/angular-frequency"]{

We consider a point-like object of mass @op{m : M} attached to a
spring whose mass we assume to be negligible. The other end of the
spring is attached to a wall. When the particle is at position
@op{x : T→L}, the force @op{F : T→F} acting on it is proportional
to the displacement @op{d : T→L} of @term{x} relative to the
spring's equilibrium length @op{l : L}:
@inset{
   @equation[force]{F = -(k × d)}
   @equation[def-d]{d = x - l}
}
where @op{k : force-constant} characterizes the elastic properties
of the spring.

Newton's equation of motion for the displacement @term{d} of the mass
takes the form
@inset{
   @equation[newton]{𝒟(𝒟(d)) = -((k ÷ m) × d)}.
}

@smaller{Additional arithmetic definitions for this context:}
@itemlist[#:style 'ordered

  @item{@smaller{
  A force constant times a length is a force:
    @linebreak[]
  @op{force-constant × L : F}
    @linebreak[]
  @op{force-constant × T→L : T→F}
    @linebreak[]
  @rule{(k × f)[t] ⇒ k × f[t]  ∀ k:force-constant  ∀ f:T→L  ∀ t:T}}}

  @item{@smaller{
  A force constant divided by a mass is the square of an angular frequency:
    @linebreak[]
  @op{force-constant ÷ M : angular-frequency-squared}}}

  @item{@smaller{
  A squared angular frequency times a length is an acceleration:
    @linebreak[]
  @op{angular-frequency-squared × L : A}
    @linebreak[]
  @op{angular-frequency-squared × T→L : T→A}
    @linebreak[]
  @rule{(ω2 × f)[t] ⇒ ω2 × f[t]  ∀ ω2:angular-frequency-squared  ∀ f:T→L  ∀ t:T}}}
]

}

@context["analytical-solution-for-mass-on-a-spring"
         #:extend "mass-on-a-spring"
         #:use "quantities/angular-frequency"]{

@section{Analytical solution}

Introducing @op{ω : angular-frequency} defined by
@equation{ω = √(k ÷ m)}, the solution of @ref[newton]  can be written as
@inset{
   @equation[solution]{d[t] = A × cos((ω × t) + δ) ∀ t:T},
}
where @op{cos(angle) : ℝ} is the cosine function. The amplitude
@op{A : L} and the phase @op{δ : angle} can take arbitray values.

@smaller{Additional arithmetic definitions for this context:}
@inset{@smaller{
  @op{√(angular-frequency-squared) : angular-frequency}}}

}

@section{Numerical solution}

@context["euler-template"
         #:insert ["quantities/function-with-finite-difference-template"]]{

For simplicity, we use one of the simplest numerical integration schemes known
as the Euler method. It was developed in the era of manual computation, where
simplicity was of utmost importance. There are much better integration schemes
today, and therefore the Euler method should @italic{not} be used in practice
when using a computer.

In the Euler method, the derivative @term{𝒟(f)} of a time-dependent quantity
@term{f} is replaced by the finite difference @term{Δ(f, h)}, where @term{h}
is a small but non-zero integration step size.
The finite difference is computed as
   @inset{@rule{Δ(f, h)[t] ⇒ (f[t + h] - f[t]) ÷ h  ∀ t:SQD}.}

}

@context["numerical-solution-for-mass-on-a-spring"
         #:extend "mass-on-a-spring"
         #:insert ["euler-template"
                   hide-vars
                   (rename-sort SQD T)
                   (rename-sort SQDnz Tnz)
                   (rename-sort SQI L)
                   (rename-sort SQInz Lnz)
                   (rename-sort SQID V)
                   (rename-sort SQIDnz Vnz)
                   (rename-sort SQD→SQI T→L)
                   (rename-sort SQD→SQID T→V)]
         #:insert ["euler-template"
                   hide-vars
                   (rename-sort SQD T)
                   (rename-sort SQDnz Tnz)
                   (rename-sort SQI V)
                   (rename-sort SQInz Vnz)
                   (rename-sort SQID A)
                   (rename-sort SQIDnz Anz)
                   (rename-sort SQD→SQI T→V)
                   (rename-sort SQD→SQID T→A)]]{

Since the Euler integration scheme applies to first-order differential
equations only, we must first transform @ref[newton] into a
set of two coupled first-order equations @ref[newton1].

We introduce @op{v : T→V} representing the velocity
of the mass. The definition of this velocity,
   @inset{@equation[newton1.d]{𝒟(d) = v},}
is the first equation in our coupled set. The second one is obtained
by applying the substitution @rule[subst newton1.d] to the 
second-order equation @ref[newton] :
  @inset{@substitute[newton1.v subst newton].}

We can now discretize the equations @ref[newton1] by applying
the substitution @rule[discretize]{𝒟(f) ⇒ Δ(f, h)},
where @var{f:Q→Q} is an arbitrary function of time
and @op{h : T} is the integration time step:

  @inset{@substitute[newtonΔ.d discretize newton1.d]
         @substitute[newtonΔ.v discretize newton1.v]}

Applying @transformation[at-t]{f → f[t]} yields the equations for an
explicit value of @op{t : T}:

  @inset{@transform[newtonΔ-t.d at-t newtonΔ.d #:reduce #t]
         @transform[newtonΔ-t.v at-t newtonΔ.v #:reduce #t]}

}

