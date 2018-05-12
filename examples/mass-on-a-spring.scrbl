#lang leibniz

@import["mechanics" "mechanics.xml"]

@title{Motion of a mass on a spring}
@author{Konrad Hinsen}

@context["mass-on-a-spring"
         #:use "mechanics/dynamics"]{

We consider a point-like object of mass @op{m : M} attached to a
spring whose mass we assume to be negligible. The other end of the
spring is attached to a wall. When the particle is at position
@op{x : T→L}, the force @op{F : T→F} acting on it is proportional
to the displacement @op{d : T→L} of @term{x} relative to the
spring's equilibrium length @op{l : L}:
@inset{
   @equation[def-d]{d = x - l} @linebreak[]
   @equation[force]{F = -(k × d)}
}
where @op{k : force-constant} characterizes the elastic properties
of the spring.

Newton's equation of motion for the position @term{x} of the mass
takes the form
@inset{
   @equation[newton-x]{m × 𝒟(𝒟(x)) = -(k × (x - l))}
}
This is a second-order ordinary differential equation, which can be
rewritten in terms of the displacement @term{d}, yielding
@inset{
   @equation[newton-d]{𝒟(𝒟(d)) = -((k ÷ m) × d)}.
}

Introducing @op{ω : angular-frequency} defined by
@equation{ω = √(k ÷ m)}, the solution can be written as
@inset{
   @equation[solution]{d[t] = A × cos((ω × t) + δ) ∀ t:T},
}
where @op{cos(ℝ) : ℝ} is the cosine function. The amplitude
@op{A : L} and the phase @op{δ : ℝ} can take arbitray values.

@smaller{Additional arithmetic definitions for this context:}
@inset{@smaller{
  @op{force-constant × T→L : T→F}
    @linebreak[]
  @op{force-constant ÷ M : angular-frequency-squared}
    @linebreak[]
  @op{√(angular-frequency-squared) : angular-frequency}
    @linebreak[]
  @op{angular-frequency-squared × T→L : T→A}
    @linebreak[]
  @op{angular-frequency × T : ℝ}
}}

}
