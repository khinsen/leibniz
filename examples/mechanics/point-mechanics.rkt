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
  sort System
  ; The sort of the collection of all masses.
  sort Masses
  ;
  ; The following two operators serve to construct a list of
  ; point masses - see example below.
  op empty-space System
  op {PointMass and System} System
  ;
  ; A selection operator that extracts one mass from the
  ; collection of all masses.
  op {Masses of PointMass} Mass
  ;
  ; The name of the collection of masses.
  op m Masses

; As an example for the usage of point-mass-system, here is the
; definition of a subset of the solar system.  With this context, the
; mass of the sun is referred to as {m of sun}. Numerical values can
; be added, here or in a separate context, via rewrite rules.

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
    op solar-system System
    => solar-system
       {sun and {earth and {moon and empty-space}}}

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

; The acceleration of each particle is required because it enters into
; Newton's law of motion.

define-context point-mass-accelerations
  ;
  include point-mass-dynamic-state
  ;
  sort Acceleration
  sort Accelerations
  ;
  op {Accelerations of PointMass} Acceleration

; Up to here, all data was defined for a single time.
; Trajectories are time-dependent versions of
; everything.

define-context point-mass-trajectory
  ;
  include point-mass-accelerations
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
  sort Force
  sort Forces
  sort ForceTrajectory
  ;
  op {Forces of PointMass} Force
  op {ForceTrajectory at Time} Forces
  ;
  op {Mass * Acceleration} Force
  op {Force / Mass} Acceleration
  ;
  op {Masses * Accelerations} Forces
  op {Forces / Masses} Accelerations
  ;
  op r Trajectory
  op f ForceTrajectory
  ;
  eq #:label law-of-motion
     ∀ I : PointMass
     ∀ T : Time
     {f at T}
     {m * {𝒟(𝒟(r)) at T}}
  ;
  ; Multiplication between Masses and Accelerations
  ; is defined per point mass.
  => ∀ M : Masses
     ∀ A : Accelerations
     ∀ I : PointMass
     {{M * A} of I}
     {{M of I} * {A of I}}
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
