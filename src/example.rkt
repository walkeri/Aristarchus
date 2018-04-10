#lang s-exp "main.rkt"

(body sun 1.989e+30 6.957e+8)

(body earth 5.972e+24 6.3781e+6)

(connection earth-sun sun earth 1.496e+11)

(surface-gravity earth)

(escape-velocity sun)

(kepler3-period earth-sun)

(L1 earth-sun #:mode 'steps)

(lagrange earth-sun)