#lang leibniz

@title{The predator-prey equations}
@author{Konrad Hinsen}

@import["functions" "functions.xml"]

@context["predator-prey" #:use "functions/ℝ→ℝ"]{

The predator-prey equations, also known as the Lotka-Volterra equations, describe the dynamics of two interacting species in an ecosystem in terms of non-linear differential equations.

The two interacting time-dependent observables are the number of prey, @op{prey : ℝ→ℝ},
and the number of predators, @op{predators : ℝ→ℝ}. Although the number of individuals of a species is really an integer, it is taken to be a real number for the benefit of using differential equations. The two coupled equations for @term{prey} and @term{predators}
are
@inset{
  @equation{pp1: 𝒟(prey) = (prey-growth-rate × prey) - predation-rate × predators × prey}
  @equation{pp2: 𝒟(predators) = (predator-growth-rate × predators × prey) - predator-loss-rate × predators}}

These equations are based on a few assumptions:
@itemlist[
  @item{In the absence of predators, the prey exihibits exponential growth described by @op{prey-growth-rate : ℝp}.}
  @item{The number of prey decreases by predation, which is @op{predation-rate : ℝp} times the number of encounters between individuals of each species. The latter is taken to be proportional to both @term{prey} and @term{predators}.}
  @item{In the absence of prey, the number of predators decreases by starvation, described by @op{predator-loss-rate : ℝp}.}
  @item{The number of predators grows with the availability of food, which, like predation, is proportional to both @term{prey} and @term{predators} with the proportionality constant @op{predator-growth-rate : ℝp}.}]

}
