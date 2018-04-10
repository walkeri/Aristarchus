#lang s-exp "untyped-main.rkt"


(body sun 1.989e+30 6.957e+8)

(body earth 5.972e+24 6.3781e+6)

(connection earth-sun sun earth 1.496e+11)


;(body-mass sun)

;(body-radius sun)

;(body-mass earth)

;(body-radius earth)

;(connection-distance earth-sun)

;(surface-gravity earth #:mode 'answer)

;(escape-velocity earth)

;(surface-gravity earth #:mode 'steps)

(L1 earth-sun #:mode 'steps)
