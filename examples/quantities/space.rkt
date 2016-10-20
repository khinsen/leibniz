#lang sweet-exp racket

provide distance distance-units space

require leibniz
        "../geometry/vector.rkt"

module+ test
  require rackunit


define-context distance
  ;
  include real-numbers
  ;
  sort Distance
  ; Non-zero is an important special case for defining division.
  sort NonZeroDistance
  subsort NonZeroDistance Distance
  ;
  ; Distances can be added ...
  op {Distance + Distance} Distance
  op {Distance - Distance} Distance
  ; and multiplied by scalars.
  op {Real * Distance} Distance
  op {NonZeroReal * NonZeroDistance} NonZeroDistance
  op {Distance / NonZeroReal} Distance
  ;
  ; The quotient of two distances is a number.
  op {Distance / NonZeroDistance} Real
  op {NonZeroDistance / NonZeroDistance} NonZeroReal
  ;
  ; Simplification rules for distances.
  => ∀ F1 : Real
     ∀ F2 : Real
     ∀ D : Distance
     {{F1 * D} + {F2 * D}}
     {{F1 + F2} * D}
  => ∀ F1 : Real
     ∀ F2 : Real
     ∀ D : Distance
     {{F1 * D} - {F2 * D}}
     {{F1 - F2} * D}
  => ∀ F1 : Real
     ∀ F2 : Real
     ∀ D : Distance
     {F1 * {F2 * D}}
     {{F1 * F2} * D}
  => ∀ F : NonZeroReal
     ∀ D : Distance
     {D / F}
     {{1 / F} * D}
  => ∀ D : Distance
     {1 * D}
     D
  ;
  ; Simplification rules for quotients of distancess
  => ∀ F : NonZeroReal
     ∀ D1 : Distance
     ∀ D2 : Distance
     {D1 / {F * D2}}
     {{D1 / F} / D2}
  => ∀ F : Real
     ∀ D1 : Distance
     ∀ D2 : Distance
     {{F * D1} / D2}
     {F * {D1 / D2}}
  => ∀ D : Distance
     {D / D}
     1


define-context distance-units
  ;
  include distance
  include boolean
  ;
  sort DistanceUnit
  subsort DistanceUnit NonZeroDistance
  ;
  op {Distance ->unit DistanceUnit} Distance
  ;
  op m DistanceUnit
  op mm DistanceUnit
  op μm DistanceUnit
  op nm DistanceUnit
  op pm DistanceUnit
  ;
  => ∀ D : Distance
     ∀ DU : DistanceUnit
     {D ->unit DU}
     {{D / DU} * DU}
  ;
  => ∀ DU1 : DistanceUnit
     ∀ DU2 : DistanceUnit
     {DU1 / DU2}
     {{DU1 / m} / {DU2 / m}}
     #:if not{DU2 == m}
  ;
  => {mm / m}
     1/1000
  => {μm / m}
     1/1000000
  => {nm / m}
     1/1000000000
  => {pm / m}
     1/1000000000000

module+ test
  ;
  with-context distance-units
    check-equal?
      RT {2 * {3 * m}}
      T  {6 * m}
    check-equal?
      RT {{2 * m} + {3 * m}}
      T  {5 * m}
    check-equal?
      RT {{2 * m} - {3 * m}}
      T  {-1 * m}
    check-equal?
      RT {{2 * m} + {3 * mm}}
      T  {{2 * m} + {3 * mm}}
    check-equal?
      RT {{{2 * m} ->unit mm} + {3 * mm}}
      T  {2003 * mm}
    check-equal?
      RT {{2 * mm} / {3 * nm}}
      T  2000000/3
    check-equal?
      RT {{2 * mm} ->unit nm}
      T  {2000000 * nm}


define-context space
  ;
  include distance
  include vector
  ;
  sort Position
  sort PositionΔ
  ;
  op {Position + PositionΔ} Position
  op {Position - PositionΔ} Position
  op {Position - Position} PositionΔ
  ;
  ; Position deltas can be added ...
  op {PositionΔ + PositionΔ} PositionΔ
  op {PositionΔ - PositionΔ} PositionΔ
  ; and multiplied by scalars.
  op {Real * PositionΔ} PositionΔ
  op { PositionΔ / NonZeroReal} PositionΔ
  ;
  ; The length of a PositionΔ is a Distance.
  op length(PositionΔ) Distance
  ;
  ; A distance times a vector is a position delta.
  op {Vector * Distance} PositionΔ
  ;
  ; A direction is a unit vector pointing from one position to another.
  op direction(Position Position) Vector
  ;
  ; Simplification rules for positions.
  => ∀ P : Position
     ∀ PD : PositionΔ
     {P - PD}
     {P + {-1 * PD}}
  => ∀ P : Position
     ∀ PD1 : PositionΔ
     ∀ PD2 : PositionΔ
     {{P + PD1} - {P + PD2}}
     {PD1 - PD2}
  => ∀ P : Position
     ∀ PD : PositionΔ
     {{P + PD} - P}
     PD
  => ∀ P : Position
     ∀ PD : PositionΔ
     {P - {P + PD}}
     {-1 * PD}
  ;
  ; Simplification rules for position deltas.
  => ∀ F1 : Real
     ∀ F2 : Real
     ∀ P : PositionΔ
     {{F1 * P} + {F2 * P}}
     {{F1 + F2} * P}
  => ∀ F1 : Real
     ∀ F2 : Real
     ∀ P : PositionΔ
     {{F1 * P} - {F2 * P}}
     {{F1 - F2} * P}
  => ∀ F1 : Real
     ∀ F2 : Real
     ∀ P : PositionΔ
     {F1 * {F2 * P}}
     {{F1 * F2} * P}
  => ∀ F : NonZeroReal
     ∀ P : PositionΔ
     {P / F}
     {{1 / F} * P}
  ;
  ; Simplification rules for lengths.
  => ∀ F : Real
     ∀ P : PositionΔ
     length{F * P}
     {F * length(P)}
  => ∀ V : Vector
     ∀ D : Distance
     length{V * D}
     {length(V) * D}

module+ test
  ;
  define-context test
    include space
    include distance-units
    op r Position
    op ex Vector
    op ey Vector
    => length(ex) 1
    => length(ey) 1
  ;
  with-context test
    check-equal?
      RT length{{r + {ex * m}} - r}
      T  m
    check-equal?
      RT length{{2 * ex} * mm}
      T  {2 * mm}
    check-equal?
      RT {{{r + {ex * m}} + {{2 * ey} * m}} - {r + {ex * m}}}
      T  {{2 * ey} * m}
