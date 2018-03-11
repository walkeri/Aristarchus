#lang s-exp "main.rkt"

(module+ test (require rackunit))

(define EPSILON 0.0001)

(body sun 1.989e+30 6.957e+8)

(body earth 5.972e+24 6.3781e+6)

(connection earth-sun sun earth 1.496e+11)

(define LoConnections1 '(earth-sun))

(system solar LoConnections1)

(module+ test
  (check-equal? (body-mass sun)
                1.989e+30)
  (check-equal? (body-radius sun)
                6.957e+8)
  (check-equal? (body-mass earth)
                5.972e+24)
  (check-equal? (body-radius earth)
                6.3781e+6)
  (check-equal? (connection-distance earth-sun)
                1.496e+11)
  (check-equal? (system-loname solar)
                '(earth-sun))
  (check-equal? (< (- (surface-gravity sun) 274.2725)  EPSILON)
                true)
  (check-equal? (< (- (surface-gravity earth) 9.807)  EPSILON)
                true)
  (check-equal? (< (- (escape-velocity earth) 11179.5684)  EPSILON)
                true)
  (check-equal? (< (- (escape-velocity sun) 617756.3135)  EPSILON)
                true)
  (check-equal? (< (- (kepler3-period earth-sun) 31554700.4807) EPSILON)
                true)
  (check-equal? (< (- (seconds->days (kepler3-period earth-sun)) 365.2164) EPSILON)
                true)
  (check-equal? (L1 earth-sun)
                (posn 148103583760.57556 0))
  (check-equal? (L2 earth-sun)
                (posn 151096416239.42444 0))
  (check-equal? (L3 earth-sun)
                (posn -149600187156.13324 0))
  (check-equal? (L4 earth-sun)
                (posn 74799550825.28027 129557400406.15201))
  (check-equal? (L5 earth-sun)
                (posn 74799550825.28027 -129557400406.15201)))

(lagrange earth-sun)




