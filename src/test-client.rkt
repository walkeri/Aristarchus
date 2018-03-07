#lang s-exp "main.rkt"

(module+ test (require rackunit))

(body sun 12 10)

(body earth 2 1)

(connection earth-sun sun earth 100)

(define LoConnections1 '(earth-sun))

(system solar LoConnections1)

(module+ test
  (check-equal? (body-mass sun)
                12)
  (check-equal? (body-radius sun)
                10)
  (check-equal? (body-mass earth)
                2)
  (check-equal? (body-radius earth)
                1)
  (check-equal? (connection-distance earth-sun)
                100)
  (check-equal? (system-loname solar)
                '(earth-sun)))
  




