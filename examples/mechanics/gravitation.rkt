#lang sweet-exp racket

provide (all-defined-out)

require leibniz
        "./point-mechanics.rkt"
        "../quantities/mass.rkt"
        "../quantities/space.rkt"
        "../quantities/time.rkt"

module+ test
  require rackunit

define-context gravitation-pair-term
  ;
  include point-mass-law-of-motion
  ;
  sort GravitationalConstant
  sort MassSquared
  sort DistanceSquared
  sort NonZeroDistanceSquared
  subsort NonZeroDistanceSquared DistanceSquared
  sort MassSquaredOverDistanceSquared
  ;
  op radial-pair-force(PointMass PointMass) Force
  ;
  op G GravitationalConstant
  op {Mass * Mass} MassSquared
  op {NonZeroDistance * NonZeroDistance} NonZeroDistanceSquared
  op sq(Distance) DistanceSquared
  op sq(NonZeroDistance) NonZeroDistanceSquared
  op {MassSquared / DistanceSquared} MassSquaredOverDistanceSquared
  op {GravitationalConstant * MassSquaredOverDistanceSquared} Force
  ;
  eq ∀ PM1 : PointMass
     ∀ PM2 : PointMass
     radial-pair-force(PM1 PM2)
     {G * {{{m of PM1} * {m of PM2}} / sq(pair-distance(PM1 PM2))}}

define-context gravitation-forces
  ;
  include gravitation-pair-term
  ;
  op no-forces Forces
  op gravitational-pair-forces(PointMass PointMass) Forces
  op gravitational-forces(PointMassSystem) Forces
  ;
  => ∀ S : PointMassSystem
     ∀ I : PointMass
     ∀ J : PointMass
     gravitational-forces{I and {J and S}}
     {gravitational-pair-forces(I J) + {gravitational-forces{I and S} + gravitational-forces{J and S}}}
  => gravitational-forces(empty-space)
     no-forces
  => ∀ I : PointMass
     gravitational-forces{I and empty-space}
     no-forces
  => ∀ F : Forces
     {F + no-forces}
     F
  => ∀ F : Forces
     {no-forces + F}
     F

module+ test
  ;
  define-context simple-solar-system
    ;
    include point-mass-system
    ;
    op sun PointMass
    op earth PointMass
    op moon PointMass
    ;
    op solar-system PointMassSystem
    => solar-system
       {sun and {earth and {moon and empty-space}}}
  ;
  define-context simple-solar-system-with-gravitation
    ;
    include simple-solar-system
    include gravitation-forces
  ;
  with-context simple-solar-system-with-gravitation
    ;
    check-equal?
      RT gravitational-forces(solar-system)
      T  {gravitational-pair-forces(sun earth) +  {gravitational-pair-forces(sun moon) +  gravitational-pair-forces(earth moon)}}
    ;
    check-equal?
      RT gravitational-forces{sun and {earth and empty-space}}
      T  gravitational-pair-forces(sun earth)
