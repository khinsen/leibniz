#lang sweet-exp racket

provide time time-units

require leibniz

module+ test
  require rackunit


define-context time
  ;
  include real-numbers
  ;
  ; Distringuish absolute times and time deltas
  sort Time
  sort TimeΔ
  ; Non-zero is a special case for defining division.
  sort NonZeroTimeΔ
  subsort NonZeroTimeΔ TimeΔ
  ;
  ; The only operation on absolute times if shifting.
  op {Time + TimeΔ} Time
  op {Time - TimeΔ} Time
  op {Time - Time} TimeΔ
  ;
  ; Time deltas can be added ...
  op {TimeΔ + TimeΔ} TimeΔ
  op {TimeΔ - TimeΔ} TimeΔ
  ; and multiplied by scalars.
  op {Real * TimeΔ} TimeΔ
  op {NonZeroReal * NonZeroTimeΔ} NonZeroTimeΔ
  op {TimeΔ / NonZeroReal} TimeΔ
  ;
  ; The quotient of two time deltas is a number.
  op {TimeΔ / NonZeroTimeΔ} Real
  op {NonZeroTimeΔ / NonZeroTimeΔ} NonZeroReal
  ;
  ; Simplification rules for times.
  => ∀ T : Time
     ∀ TD : TimeΔ
     {T - TD}
     {T + {-1 * TD}}
  => ∀ T : Time
     ∀ TD1 : TimeΔ
     ∀ TD2 : TimeΔ
     {{T + TD1} - {T + TD2}}
     {TD1 - TD2}
  => ∀ T : Time
     ∀ TD : TimeΔ
     {{T + TD} - T}
     TD
  => ∀ T : Time
     ∀ TD : TimeΔ
     {T - {T + TD}}
     {-1 * TD}
  ;
  ; Simplification rules for time deltas.
  => ∀ F1 : Real
     ∀ F2 : Real
     ∀ T : TimeΔ
     {{F1 * T} + {F2 * T}}
     {{F1 + F2} * T}
  => ∀ F1 : Real
     ∀ F2 : Real
     ∀ T : TimeΔ
     {{F1 * T} - {F2 * T}}
     {{F1 - F2} * T}
  => ∀ F1 : Real
     ∀ F2 : Real
     ∀ T : TimeΔ
     {F1 * {F2 * T}}
     {{F1 * F2} * T}
  => ∀ F : NonZeroReal
     ∀ T : TimeΔ
     {T / F}
     {{1 / F} * T}
  => ∀ T : TimeΔ
     {1 * T}
     T
  ;
  ; Simplification rules for quotients of time deltas
  => ∀ F : NonZeroReal
     ∀ T1 : TimeΔ
     ∀ T2 : TimeΔ
     {T1 / {F * T2}}
     {{T1 / F} / T2}
  => ∀ F : Real
     ∀ T1 : TimeΔ
     ∀ T2 : TimeΔ
     {{F * T1} / T2}
     {F * {T1 / T2}}
  => ∀ T : TimeΔ
     {T / T}
     1


define-context time-units
  ;
  include time
  include boolean
  ;
  sort TimeUnit
  subsort TimeUnit NonZeroTimeΔ
  ;
  op {TimeΔ ->unit TimeUnit} TimeΔ
  ;
  op s TimeUnit
  op ms TimeUnit
  op μs TimeUnit
  op ns TimeUnit
  op ps TimeUnit
  ;
  => ∀ T : TimeΔ
     ∀ TU : TimeUnit
     {T ->unit TU}
     {{T / TU} * TU}
  ;
  => ∀ TU1 : TimeUnit
     ∀ TU2 : TimeUnit
     {TU1 / TU2}
     {{TU1 / s} / {TU2 / s}}
     #:if not{TU2 == s}
  ;
  => {ms / s}
     1/1000
  => {μs / s}
     1/1000000
  => {ns / s}
     1/1000000000
  => {ps / s}
     1/1000000000000

module+ test
  define-context test
    include time-units
    op t Time
  ;
  with-context test
    check-equal?
      RT {2 * {3 * s}}
      T  {6 * s}
    check-equal?
      RT {{2 * s} + {3 * s}}
      T  {5 * s}
    check-equal?
      RT {{2 * s} - {3 * s}}
      T  {-1 * s}
    check-equal?
      RT {{2 * s} + {3 * ms}}
      T  {{2 * s} + {3 * ms}}
    check-equal?
      RT {{{2 * s} ->unit ms} + {3 * ms}}
      T  {2003 * ms}
    check-equal?
      RT {{2 * ms} / {3 * ns}}
      T  2000000/3
    check-equal?
      RT {{2 * ms} ->unit ns}
      T  {2000000 * ns}
    check-equal?
      RT {{t + {2 * ms}} - {t - {3 * ms}}}
      T  {5 * ms}
    check-equal?
      RT {t - {t - s}}
      T  s
