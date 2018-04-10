#lang racket

(require "formula.rkt"
         "built-in-structures.rkt")

(provide

 (all-from-out "built-in-structures.rkt")
 
 body
 connection
 system
 
 ;; Name -> Number
 ;; Calculates the surface gravity on a body 
 surface-gravity
 
 ;; Name -> Number
 ;; Calculates the escape velocity from a body 
 escape-velocity

 ;; Connection -> Number
 ;; Calculates the period of orbit of a connection
 kepler3-period
 
 ;; Connection -> Posn
 ;; Calculates the L1 lagrange point of a connection
 L1
 
 ;; Connection -> Posn
 ;; Calculates the L2 lagrange point of a connection
 L2

 ;; Connection -> Posn
 ;; Calculates the L3 lagrange point of a connection
 L3

 ;; Connection -> Posn
 ;; Calculates the L3 lagrange point of a connection
 L4

 ;; Connection -> Posn
 ;; Calculates the L3 lagrange point of a connection
 L5
)

;; The gravitational constant
(define G 6.6740831e-11)

;; The speed of light
(define c 299792458)

(formula (surface-gravity body)
         ((G G) (M_b (body-mass body)) (r (body-radius body)))
         (/ (* G M_b) (expt r 2)))

(formula (escape-velocity body)
         ((G G) (M_b (body-mass body)) (r (body-radius body)))
         (sqrt (/ (* 2 G M_b) r)))

(formula (kepler3-period conn)
         ((G G) (M (max (body-mass (connection-name1 conn)) (body-mass (connection-name2 conn))))
                (r (connection-distance conn)) (\\pi pi))
         (sqrt (/ (* 4 (expt \\pi 2) (expt r 3))
                  (* G M))))

(formula (L1 connect)
         ((\\alpha (/ (min (body-mass (connection-name1 connect))
                           (body-mass (connection-name2 connect)))
                      (+ (max (body-mass (connection-name1 connect))
                              (body-mass (connection-name2 connect)))
                         (min (body-mass (connection-name1 connect))
                              (body-mass (connection-name2 connect))))))
          (R (connection-distance connect)))
         (posn (* R (- 1 (expt (/ \\alpha 3) (/ 1 3))))
               0))

(formula (L2 connect)
         ((\\alpha (/ (min (body-mass (connection-name1 connect))
                           (body-mass (connection-name2 connect)))
                      (+ (max (body-mass (connection-name1 connect))
                              (body-mass (connection-name2 connect)))
                         (min (body-mass (connection-name1 connect))
                              (body-mass (connection-name2 connect))))))
          (R (connection-distance connect)))
         (posn (* R (+ 1 (expt (/ \\alpha 3) (/ 1 3))))
               0))

(formula (L3 connect)
         ((\\alpha (/ (min (body-mass (connection-name1 connect))
                           (body-mass (connection-name2 connect)))
                      (+ (max (body-mass (connection-name1 connect))
                              (body-mass (connection-name2 connect)))
                         (min (body-mass (connection-name1 connect))
                              (body-mass (connection-name2 connect))))))
          (R (connection-distance connect)))
         (posn (* (* -1 R) (+ 1 (* (/ 5 12) \\alpha)))
               0))

(formula (L4 connect)
         ((M1 (max (body-mass (connection-name1 connect))
                              (body-mass (connection-name2 connect))))
          (M2 (min (body-mass (connection-name1 connect))
                   (body-mass (connection-name2 connect))))
          (R (connection-distance connect)))
         (posn (* (/ R 2)
                  (/ (- M1 M2)
                     (+ M1 M2)))
               (* (/ (sqrt 3) 2) R)))

(formula (L5 connect)
         ((M1 (max (body-mass (connection-name1 connect))
                              (body-mass (connection-name2 connect))))
          (M2 (min (body-mass (connection-name1 connect))
                   (body-mass (connection-name2 connect))))
          (R (connection-distance connect)))
         (posn (* (/ R 2)
                  (/ (- M1 M2)
                     (+ M1 M2)))
               (* -1 (/ (sqrt 3) 2) R)))