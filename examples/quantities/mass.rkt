#lang sweet-exp racket

provide mass mass-units

require leibniz

module+ test
  require rackunit

; The context "mass" defines the physical concept of a mass
; from a computational point of view: it defines the operations
; that can be applied to representations of masses.
;
; Note that the use of PositiveReal implies that masses can neither be
; negative nor zero. In a computational context, that is usually a
; reasonable choice, because negative or zero masses are most likely
; to result from mistakes.

define-context mass
  ;
  include real-numbers
  ;
  sort Mass
  ;
  ; The sum of two masses is a mass.
  op {Mass + Mass} Mass
  ; The product of a positive number with a mass is a mass.
  op {PositiveReal * Mass} Mass
  ; A mass divided by a positive number is a mass.
  op {Mass / PositiveReal} Mass
  ; The quotient of two masses is a positive number.
  op {Mass / Mass} PositiveReal
  ;
  ; Reduce terms of type mass to an unreducible mass or to the form
  ; {factor * unreducible mass}.
  => #:vars ( (F1 PositiveReal) (F2 PositiveReal) (M Mass) )
     {F1 * {F2 * M}}
     {{F1 * F2} * M}
  => #:vars ( (F PositiveReal) (M  Mass) )
     {M / F}
     {{1 / F} * M}
  ; Reduce quotients of two masses to a number or to the form
  ; {factor * {unreducible mass / unreducible mass}}.
  => #:vars ( (F PositiveReal) (M1  Mass) (M2 Mass) )
     {M1 / {F * M2}}
     {{M1 / F} / M2}
  => #:vars ( (F PositiveReal) (M1  Mass) (M2 Mass) )
     {{F * M1} / M2}
     {F * {M1 / M2}}
  => #:var (M  Mass)
     {M / M}
     1

; The context "mass-units" adds the concept of a mass unit and
; unit conversion facilities. Unit conversion is never automatic,
; but must be requested explicitly.
;
; Any mass unit can be converted to any other one, but conversion
; factors are stored only from each unit to an arbitrarily
; chosen pivot unit, which is kg in this implementation. This
; pivot unit is not special in any other way.

define-context mass-units
  ;
  include mass
  ; The boolean context provides the "not" operator that is used in a
  ; conditional rule below.
  include boolean
  ;
  sort MassUnit
  subsort MassUnit Mass
  ;
  ; A mass converted to another unit is a mass as well.
  op {Mass ->unit MassUnit} Mass
  ;
  ; The available units - more can be added easily.
  op kg MassUnit
  op  g MassUnit
  op mg MassUnit
  ;
  ; Unit conversion relies on the simplication rules from context
  ; "mass", which reduce M / MU to a number if all conversion factors
  ; have been defined properly.
  => #:vars ( (M Mass) (MU MassUnit) )
     {M ->unit MU}
     {{M / MU} * MU}
  ; The quotient of two mass units is reduced to the quotient of their
  ; conversion factors with respect to the pivot unit kg. The
  ; condition ensures that the rule is not applied if the denominator
  ; is kg, otherwise this would create an infinite rewrite loop.
  => #:vars ( (MU1 MassUnit) (MU2 MassUnit) )
     {MU1 / MU2}
     {{MU1 / kg} / {MU2 / kg}}
     #:if not{MU2 == kg}
  ;
  ; Conversion factors are specified as rules for mass unit quotients
  ; with the pivot unit in the denominator - the ones that were
  ; excluded from the general quotient rule by the added condition.
  => {g / kg}
     1/1000
  => {mg / kg}
     1/1000000

; The unit tests illustrate computations with masses and mass units.
; The lines starting with RT give the initial terms, which are reduced
; to the term given in the following line starting with T.
;
; T stands for "term"; it marks the following expression as a Leibniz
; term rather than a Racket expression. RT is a shorthand for
; reduce(T ...).

module+ test
  ;
  with-context mass-units
    check-equal?
      RT {2 * {3 * kg}}
      T  {6 * kg}
    check-equal?
      RT {{2 * kg} / 3}
      T  {2/3 * kg}
    check-equal?
      RT {{2 * kg} / {3 * kg}}
      T  2/3
    check-equal?
      RT {{2 * g} / {3 * kg}}
      T  2/3000
    check-equal?
      RT {{2 * g} / {3 * mg}}
      T  2000/3
    check-equal?
      RT {{2 * g} ->unit mg}
      T  {2000 * mg}
