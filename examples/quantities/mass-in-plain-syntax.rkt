#lang racket

; This is a version of the mass and mass-units contexts written in
; plain Racket syntax, rather than sweet-exp syntax. It serves mainly
; to illustrate that syntax is a superficial feature of Leibniz at
; this time.

(provide mass mass-units)

(require leibniz)

(module+ test
  (require rackunit))

(define-context mass

  (include real-numbers)

  (sort Mass)

  (op (Mass . + . Mass) Mass)
  (op (PositiveReal . * . Mass) Mass)
  (op (Mass . / . PositiveReal) Mass)
  (op (Mass . / . Mass) PositiveReal)

  (=> #:vars ([F1 PositiveReal] [F2 PositiveReal] [M Mass])
      (F1 . * . (F2 . * . M))
      ((F1 . * . F2) . * . M))
  (=> #:vars ([F PositiveReal] [M  Mass])
      (M . / . F)
      ((1 . / . F) . * . M))
  (=> #:vars ([F PositiveReal] [M1  Mass] [M2 Mass])
      (M1 . / . (* F M2))
      ((/ M1 F) . / . M2))
  (=> #:vars ([F PositiveReal] [M1  Mass] [M2 Mass])
      ((F . * . M1) . / . M2)
      (F . * . (/ M1 M2)))
  (=> #:var (M Mass)
      (M . / . M)
      1))

(define-context mass-units
  (include mass)
  (include boolean)

  (sort MassUnit)
  (subsort MassUnit Mass)

  (op (Mass . ->unit . MassUnit) Mass)

  (op kg MassUnit)
  (op  g MassUnit)
  (op mg MassUnit)

  (=> #:vars ([M Mass] [MU MassUnit])
      (M . ->unit . MU)
      ((M . / . MU) . * . MU))
  (=> #:vars ([MU1 MassUnit] [MU2 MassUnit])
      (MU1 . / . MU2)
      ((MU1 . / . kg) . / . (MU2 . / . kg))
      #:if (not (== MU2 kg)))

  (=> (g . / . kg)
      1/1000)
  (=> (mg . / . kg)
      1/1000000))

(module+ test
  (with-context mass-units
    (check-equal?
     (RT (2 . * . (3 . * . kg)))
     (T  (6  . * . kg)))
    (check-equal?
     (RT ((* 2 kg) . / . 3))
     (T  (2/3 . * . kg)))
    (check-equal?
     (RT ((2 . * . kg) . / . (3 . * . kg)))
     (T  2/3))
    (check-equal?
     (RT ((2 . * . g) . / . (3 . * . kg)))
     (T  2/3000))
    (check-equal?
     (RT ((2 . * . g) . / . (3 . * . mg)))
     (T  2000/3))
    (check-equal?
     (RT ((2 . * . g)  . ->unit . mg))
     (T  (2000 . * . mg)))))
