#lang sweet-exp racket

provide (all-defined-out)

require leibniz
        "../quantities/mass.rkt"
        "../quantities/space.rkt"
        "../quantities/time.rkt"

module+ test
  require rackunit

; A point mass system is defined by naming each point mass.
; A mass is associated with each name.

define-context point-mass-system
  ;
  include mass
  ;
  ; The sort of point mass identifiers.
  sort PointMass
  ; The sort of the whole system.
  sort PointMassSystem
  ; A single point mass can be a system by itself
  subsort PointMass PointMassSystem
  ; The sort of the collection of all masses.
  sort Masses
  ;
  ; empty-space is the degenerate case of an empty point-mass system
  op empty-space PointMassSystem
  ; Any combination of point-mass systems is a point-mass system
  op {PointMassSystem and PointMassSystem} PointMassSystem
  ;
  ; A selection operator that extracts one mass from the
  ; collection of all masses.
  op {Masses of PointMass} Mass
  ;
  ; The name of the collection of masses.
  op m Masses
  ;
  ; Simplification rules:
  ; 1) Eliminate empty-space
  => ∀ PM : PointMassSystem
     {PM and empty-space}
     PM
  => ∀ PM : PointMassSystem
     {empty-space and PM}
     PM
  ; 2) Normalize combinations
  => ∀ PM1 : PointMassSystem
     ∀ PM2 : PointMassSystem
     ∀ PM3 : PointMassSystem
     {{PM1 and PM2} and PM3}
     {PM1 and {PM2 and PM3}}

; As an example for the usage of point-mass-system, here is the
; definition of a subset of the solar system.  With this context, the
; mass of the sun is referred to as {m of sun}. Numerical values can
; be added, here or in a separate context, via rewrite rules.

module+ test
  ;
  define-context inner-planets
    ;
    include point-mass-system
    ;
    op sun PointMass
    op mercury PointMass
    op venus PointMass
    op earth PointMass
    ;
    op planets PointMassSystem
    op solar-system PointMassSystem
    ;
    => planets
       {{mercury and venus} and earth}
    => solar-system
       {sun and planets}
  ;
  with-context inner-planets
    check-equal?
      RT solar-system
      T  {sun and {mercury and {venus and earth}}}
    
; The next context adds a configuration, i.e. a position for each
; point mass.

define-context point-mass-configuration
  ;
  include point-mass-system
  include space
  ;
  sort Positions
  ;
  op {Positions of PointMass} Position
  ;
  op pair-distance(PointMass PointMass) NonZeroDistance

; The dynamic state of a point mass system consists of the
; configuration plus a velocity for each point mass.

define-context point-mass-dynamic-state
  ;
  include point-mass-configuration
  ;
  sort Velocity
  sort Velocities
  sort DynamicState
  ;
  op State(Positions Velocities) DynamicState
  ;
  op {Velocities of PointMass} Velocity

; Accelerations and forces are needed for Newton's law of motion

define-context point-mass-forces
  ;
  include point-mass-dynamic-state
  ;
  sort Acceleration
  sort Force
  ;
  sort Accelerations
  sort Forces
  ;
  op no-force Force
  op no-forces Forces
  ;
  op {Accelerations of PointMass} Acceleration
  op {Forces of PointMass} Force
  ;
  op {Force + Force} Force
  op {Forces + Forces} Forces
  ;
  op {Real * Force} Force
  op {Vector * Force} Force
  ;
  op {Mass * Acceleration} Force
  op {Force / Mass} Acceleration
  ;
  op {Masses * Accelerations} Forces
  op {Forces / Masses} Accelerations
  ;
  ; The relation between Masses, Accelerations, and Forces is defined
  ; per point mass.
  => ∀ M : Masses
     ∀ A : Accelerations
     ∀ I : PointMass
     {{M * A} of I}
     {{M of I} * {A of I}}
  => ∀ M : Masses
     ∀ F : Forces
     ∀ I : PointMass
     {{F / M} of I}
     {{F of I} / {M of I}}
  ;
  ; Simplification rules for force-mass-acceleration
  => ∀ M : Mass
     ∀ A : Acceleration
     {{M * A} / M}
     A
  => ∀ M : Mass
     ∀ F : Force
     {M * {F / M}}
     F
  ;
  ; Simplification rules for no-force(s)
  => ∀ F : Force
     {F + no-force}
     F
  => ∀ F : Force
     {no-force + F}
     F
  => ∀ F : Forces
     {F + no-forces}
     F
  => ∀ F : Forces
     {no-forces + F}
     F

; Up to here, all data was defined for a single time.
; Trajectories are time-dependent versions of
; everything.

define-context point-mass-trajectory
  ;
  include point-mass-forces
  include time
  ;
  sort Trajectory
  sort VelocityTrajectory
  sort AccelerationTrajectory
  ;
  op {Trajectory at Time} Positions
  op {VelocityTrajectory at Time} Velocities
  op {AccelerationTrajectory at Time} Accelerations
  ;
  op 𝒟(Trajectory) VelocityTrajectory
  op 𝒟(VelocityTrajectory) AccelerationTrajectory

; Now we can write down Newton's law of motion.

define-context point-mass-law-of-motion
  ;
  include point-mass-trajectory
  ;
  sort ForceTrajectory
  ;
  op {ForceTrajectory at Time} Forces
  ;
  op r Trajectory
  op f ForceTrajectory
  ;
  eq #:label law-of-motion
     ∀ T : Time
     {f at T}
     {m * {𝒟(𝒟(r)) at T}}
