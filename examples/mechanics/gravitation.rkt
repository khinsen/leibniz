#lang sweet-exp racket

provide (all-defined-out)

require leibniz
        "./point-mechanics.rkt"
        "../quantities/mass.rkt"
        "../quantities/space.rkt"
        "../quantities/time.rkt"

module+ test
  require rackunit

define-context point-mass-pair-interactions
  ;
  include point-mass-forces
  ;
  op pair-forces(PointMassSystem Positions) Forces
  op radial-pair-force(PointMass PointMass Positions) Force
  ;
  ; No force between a particle and itself.
  => ∀ PM : PointMass
     ∀ R : Positions
     {pair-forces(PM R) of PM}
     no-force
  ; For any other pair, it's the radial part times the direction.
  => ∀ PM1 : PointMass
     ∀ PM2 : PointMass
     ∀ R : Positions
     {pair-forces(PM1 R) of PM2}
     {direction({R of PM2} {R of PM1}) *  radial-pair-force(PM1 PM2 R)}
  ; For composite systems, recurse for both subsystems.
  => ∀ PM : PointMass
     ∀ S1 : PointMassSystem
     ∀ S2 : PointMassSystem
     ∀ R : Positions
     {pair-forces({S1 and S2} R) of PM}
     {{pair-forces(S1 R) of PM} + {pair-forces(S2 R) of PM}}

module+ test
  ;
  define-context simple-solar-system-configuration
    ;
    include simple-solar-system
    include point-mass-configuration
    ;
    op r Positions
  ;
  define-context simple-solar-system-pair-interactions
    ;
    include simple-solar-system-configuration
    include point-mass-pair-interactions
  ;
  with-context simple-solar-system-pair-interactions
    ;
    check-equal?
      RT {pair-forces({sun and earth} r) of sun}
      T  {direction({r of sun} {r of earth}) * radial-pair-force(earth sun r)}
    ;
    check-equal?
      RT {pair-forces(solar-system r) of earth}
      T  {{direction({r of earth} {r of sun}) * radial-pair-force(sun earth r)} + {direction({r of earth} {r of moon}) * radial-pair-force(moon earth r)}}
