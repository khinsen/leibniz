#lang sweet-exp racket

provide (all-defined-out)

require leibniz
        "./point-mechanics.rkt"
        "./gravitation.rkt"
        "../geometry/vector.rkt"
        "../quantities/mass.rkt"
        "../quantities/space.rkt"
        "../quantities/time.rkt"

module+ test
  require rackunit

define-context sun-earth-system
  ;
  include point-mass-configuration
  ;
  op sun PointMass
  op earth PointMass
  ;
  op sun-earth-system PointMassSystem
  ;
  op r Positions
  ;
  => sun-earth-system
     {sun and earth}

define-context astronomical-units
  ;
  include distance-units
  include mass-units
  include time-units
  ;
  op au DistanceUnit
  op solar-mass MassUnit
  op day TimeUnit
  ;
  => {au / m}  149597870700
  => {solar-mass / kg}  #e1.98892e30
  => {day / s} 86400

define-context sun-earth-system-numerical-values
  ;
  include sun-earth-system
  include point-mass-gravitation
  include astronomical-units
  include vector-3d
  ;
  op O Position ; origin of the coordinate system
  ;
  => {mass of sun}   solar-mass
  => {mass of earth} {solar-mass / 332950}
  ;
  => {r of sun}  {O + {(V #e4.29643147e-3 #e-2.98842489e-4 #e-1.03361097e-4) * au}}
  => {r of earth}  {O + {(V #e-0.17664088 #e0.96621028 #e-1.1652706e-4) * au}}
  ;
  => ∀ D : Distance
     sq(D)
     {D * D}
  => ∀ P1 : Position
     ∀ P2 : Position
     distance(P1 P2)
     length{P1 - P2}

; Add some ad-hoc simplification rules for gravitational
; force computations. Such rules should in the long run
; become part of a general simplification algorithm.

define-context sun-earth-system-with-simplification
  ;
  include sun-earth-system-numerical-values
  ;
  => ∀ DU : DistanceUnit
     ∀ F1 : NonZeroReal
     ∀ F2 : NonZeroReal
     {{F1 * DU} * {F2 * DU}}
     {{F1 * F2} * {DU * DU}}
  ;
  => ∀ X : NonNegativeReal
     {√(X) * √(X)}
     X
  ;
  => ∀ R2 : NonZeroReal
     ∀ M : Mass
     {G * {{M * solar-mass} / {R2 * {au * au}}}}
     {M * {{{#e2.9591221287226995e-4 / R2} * au} / {day * day}}}
  => ∀ M : Mass
     {solar-mass * M}
     {M * solar-mass}
  => ∀ A : Real
     ∀ B : Real
     {A / √(B)}
     √{{A * A} / B}
  => ∀ A : Real
     ∀ B : Real
     ∀ C : Real
     {{A * B} + {A * C}}
     {A * {B + C}}

; Now we can check that the sum of the forces on the point masses is zero.

module+ test
  ;
  with-context sun-earth-system-with-simplification
    ;
    check-equal?
    RT {{pair-forces({sun and earth} r) of sun} + {pair-forces({sun and earth} r) of earth}}
    T  no-force
