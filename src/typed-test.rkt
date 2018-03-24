#lang s-exp "typed-main.rkt"

(require turnstile/rackunit-typechecking)

(define EPSILON (assign-unit 0.0001 number))

(body sun (assign-unit 1.989e+30 mass) (assign-unit 6.957e+8 distance))

(body earth (assign-unit 5.972e+24 mass) (assign-unit 6.3781e+6 distance))

(connection earth-sun earth sun (assign-unit 1.496e+11 distance))

(define LoConnections1 '(earth-sun))

(system solar LoConnections1)

(check-type (body-mass sun) : mass -> 1.989e+30)
(check-type (body-radius sun) : distance -> 6.957e+8)
(check-type (connection-name1 earth-sun) : name -> earth)
(check-type (connection-name2 earth-sun) : name -> sun)
(check-type (connection-distance earth-sun) : distance -> 1.496e+11)
(check-type (surface-gravity earth) : accel)
(check-type (escape-velocity earth) : vel)
(check-type (kepler3-period earth-sun) : time)
(check-type (L1 earth-sun) : coordinate)
(check-type (L2 earth-sun) : coordinate)
(check-type (L3 earth-sun) : coordinate)
(check-type (L4 earth-sun) : coordinate)
(check-type (L5 earth-sun) : coordinate)
(check-type (lagrange earth-sun) : void)


