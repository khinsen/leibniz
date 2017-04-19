
#lang leibniz

@title{Masses and mass units}
@author{Konrad Hinsen}

@import["boolean" "boolean.xml"]

@context["mass" #:use "builtins/real-numbers"]{

@section{Masses}

The sum of two @sort{mass}es is a @sort{mass}:
  @inset{@op{mass + mass : mass}}

The product of a positive number with a @sort{mass} is a @sort{mass}:
  @inset{@op{ℝp × mass : mass}}

A @sort{mass} divided by a positive number is a @sort{mass}:
  @inset{@op{mass ÷ ℝp : mass}}

The quotient of two @sort{mass}es is a positive number:
  @inset{@op{mass ÷ mass : ℝp}}

@subsection{Simplification rules}

In the following, we use the variables @var{M:mass}, @var{M1:mass}, @var{M2:mass}
and @var{F:ℝp}, @var{F1:ℝp}, @var{F2:ℝp}.

Combine multiples of the same mass:
  @inset{@rule{(F1 × M) + (F2 × M) ⇒ (F1 + F2) × M}}

Replace multiple prefactors and divisions by simple prefactors:
  @inset{@rule{F1 × (F2 × M) ⇒ (F1 × F2) × M}
         @rule{M ÷ F ⇒ (1 ÷ F) × M}}

Reduce quotients of two @sort{mass}es to a number if possible:
  @inset{@rule{M1 ÷ (F × M2) ⇒ (M1 ÷ F) ÷ M2}
         @rule{(F × M1) ÷ M2 ⇒ F × (M1 ÷ M2)}
         @rule{M ÷ M ⇒ 1}}
}

@context["mass-units" #:extend "mass" #:use "boolean/boolean"]{

@section{Mass units}

A @sort{mass-unit ⊆ mass} is a @sort{mass} used as a reference in specifying other masses.

Some common mass units are:
  @inset{@op{kg : mass-unit}
         @op{g : mass-unit}
         @op{mg : mass-unit}}

A mass converted to another unit is a mass as well:
  @inset{@op{mass in mass-unit : mass}}

Mass conversion is done in two steps. First all masses are expressed in
terms of a @italic{pivot unit}, which is the @term{kg}. Next, the result
is expressed in terms of the desired unit. Due to this two-step process,
conversion factors must only be specified between each unit and the
pivot unit:
  @inset{@rule{g ÷ kg ⇒ 1/1000}
         @rule{mg ÷ kg ⇒ 1/1000000}}

@subsection{Simplification rules}

Additional variables: @var{MU:mass-unit}, @var{MU1:mass-unit}, @var{MU2:mass-unit}.

The following rule achieves unit conversion in concertation with the mass
simplification rules, which reduce the quotient to a number with the help
of the conversion factors:
  @inset{@rule{M in MU ⇒ (M ÷ MU) × MU}}

Moreover, the quotient of two mass units is reduced to the quotient of their
conversion factors with respect to the pivot unit:
  @inset{@rule{MU1 ÷ MU2 ⇒ (MU1 ÷ kg) ÷ (MU2 ÷ kg)
         if ¬(MU2 == kg)}}

@subsection{Tests}
  @inset{@test{2 × (3 × kg) ⇒ 6 × kg}
         @test{2 × (kg ÷ 3) ⇒ 2/3 × kg}
         @test{(2 × kg) ÷ 3 ⇒ 2/3 × kg}
         @test{(2 × kg) ÷ (3 × kg) ⇒ 2/3}
         @test{(2 × g) ÷ (3 × kg) ⇒ 2/3000}
         @test{(2 × g) ÷ (3 × mg) ⇒ 2000/3}
         @test{(2 × g) in mg ⇒ 2000 × mg}}

}
