#lang sweet-exp racket

provide vector vector-3d

require leibniz

module+ test
  require rackunit

; Note: This is *not* a general definition for a vector space, but a
; specialized definition for use in Euclidean geometry.
;
define-context vector
  ;
  include real-numbers
  ;
  sort Vector
  ;
  op {Vector + Vector} Vector
  op {Vector - Vector} Vector
  op {Real * Vector} Vector
  op {Vector / NonZeroReal} Vector
  ;
  op {Vector * Vector} Real
  op length(Vector) NonNegativeReal
  ;
  ; Simplification rules
  ;
  => ∀ V1 : Vector
     ∀ V2 : Vector
     {V1 - V2}
     {V1 + {-1 * V2}}
  => ∀ F1 : Real
     ∀ F2 : Real
     ∀ V : Vector
     {{F1 * V} + {F2 * V}}
     {{F1 + F2} * V}
  => ∀ F : Real
     ∀ V : Vector
     {{F * V} + V}
     {{F + 1} * V}
  => ∀ F : Real
     ∀ V : Vector
     {V + {F * V}}
     {{F + 1} * V}
  => ∀ F : NonZeroReal
     ∀ V : Vector
     {V / F}
     {{1 / F} * V}
  ;
  => ∀ V : Vector
     {1 * V}
     V
  ;
  => ∀ F1 : Real
     ∀ F2 : Real
     ∀ V1 : Vector
     ∀ V2 : Vector
     {{F1 * V1} * {F2 * V2}}
     {{F1 * F2} * {V1 * V2}}
  => ∀ F : Real
     ∀ V1 : Vector
     ∀ V2 : Vector
     {{F * V1} * V2}
     {F * {V1 * V2}}
  => ∀ F : Real
     ∀ V1 : Vector
     ∀ V2 : Vector
     {V1 * {F * V2}}
     {F * {V1 * V2}}
  ;
  => ∀ F1 : Real
     ∀ F2 : Real
     ∀ V : Vector
     {F1 * {F2 * V}}
     {{F1 * F2} * V}
  ;
  => ∀ F : Real
     ∀ V : Vector
     length{F * V}
     {abs(F) * length(V)}

module+ test
  ;
  define-context test1
    include vector
    op ex Vector
    op ey Vector
    => {ex * ey} 0
    => {ex * ex} 1
    => {ey * ey} 1
    => length(ex) 1
    => length(ey) 1
  ;
  with-context test1
    check-equal?
      RT length{2 * ex}
      T  2
    check-equal?
      RT {{2 * ex} - {3 * ex}}
      T  {-1 * ex}
    check-equal?
      RT {ex - ex}
      T  {0 * ex}

define-context vector-3d
  ;
  include vector
  ;
  sort Vector3D
  subsort Vector3D Vector
  ;
  op V(Real Real Real) Vector3D
  op V(Real Real Real) Vector3D
  ;
  => ∀ X : Real
     ∀ Y : Real
     ∀ Z : Real
     ∀ A : Real
     ∀ B : Real
     ∀ C : Real
     {V(X Y Z) + V(A B C)}
     V({X + A} {Y + B} {Z + C})
  => ∀ X : Real
     ∀ Y : Real
     ∀ Z : Real
     ∀ F : Real
     {F * V(X Y Z)}
     V({F * X} {F * Y} {F * Z})
  => ∀ X : Real
     ∀ Y : Real
     ∀ Z : Real
     ∀ A : Real
     ∀ B : Real
     ∀ C : Real
     {V(X Y Z) * V(A B C)}
     {{{X * A} + {Y * B}} + {Z * C}}
  ;
  => ∀ X : Real
     length(V(X 0 0))
     abs(X)
  => ∀ Y : Real
     length(V(0 Y 0))
     abs(Y)
  => ∀ Z : Real
     length(V(0 0 Z))
     abs(Z)
  => ∀ X : Real
     ∀ Y : Real
     ∀ Z : Real
     length(V(X Y Z))
     √{{X * X} + {{Y * Y} + {Z * Z}}}

module+ test
  ;
  define-context test2
    include vector-3d
    include test1
    => ex V(1 0 0)
    => ey V(0 1 0)
  ;
  with-context test2
    check-equal?
      RT length{2 * ex}
      T  2
    check-equal?
      RT {{2 * ex} - {3 * ex}}
      T  V(-1 0 0)
    check-equal?
      RT {{2 * ex} * {3 * ex}}
      T  6
    check-equal?
      RT {{2 * ex} * {3 * ey}}
      T  0
    check-equal?
      RT {V(0 0 1) * ex}
      T  0
