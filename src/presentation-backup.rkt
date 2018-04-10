#lang racket

;; Body -> Number
;; Calculates the radius of the resulting black hole that
;; would result from the gravitational collapse of a body
(provide schwarzchild-radius)

(formula (schwarzchild-radius body)
         ((G G) (M_b (body-mass body)) (c c))
         (/ (* 2 G M_b) (expt c 2)))

;; Remember to provide it

