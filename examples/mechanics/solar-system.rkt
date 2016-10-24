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

define-context sun-earth-gravitation
  ;
  include sun-earth-system
  include point-mass-gravitation

with-context sun-earth-system-numerical-values
  ;
  displayln
    RT {pair-forces({sun and earth} r) of sun}

;; Sun
;; 0.642737 -0.0447062 -0.0154626
;; 0.000104211 0.001252 -1.30241e-05
;; 1.98864e+06
;; 0.696
;; Earth
;; -26.4251 144.543 -0.0174322
;; -2.97599 -0.556919 6.52213e-05
;; 5.9722
;; 0.00637101
