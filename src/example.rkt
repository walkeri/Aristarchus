#lang s-exp "untyped-main.rkt"


(body sun 1.989e+30 6.957e+8)

(body earth 5.972e+24 6.3781e+6)

(connection earth-sun sun earth 1.496e+11)


(system solar (earth-sun))

(body-mass sun)

(body-radius sun)

(connection-distance earth-sun)


(surface-gravity sun)

(surface-gravity earth)

(escape-velocity earth)

(kepler3-period earth-sun)

(L1 earth-sun)
(L2 earth-sun)
(L3 earth-sun)
(L4 earth-sun)
(L5 earth-sun)

(surface-gravity earth
                 #:mode 'webpage)