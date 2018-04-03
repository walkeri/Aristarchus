#lang racket

(require (for-syntax syntax/parse)
         (prefix-in un: racket)
         htdp/matrix)

(provide

 (all-from-out racket)

 ;;------------------------------ Definitions ----------------------------------

 ;; Number Number -> #<procedure:posn>
 posn
 
 ;; Name Number Number -> #<procedure:Name>
 ;; Creates a body with a mass and radius
 body

 ;; Body -> Number
 ;; Gets the mass of a body
 body-mass

 ;; Body -> Number
 ;; Gets the radius of a body
 body-radius
 
 
 ;; Name Name Name Number -> #<procedure:Name>
 ;; Creates a connection between two bodies
 connection

 ;; Connection -> Name
 ;; Gets the name of the first body
 connection-name1

 ;; Connection -> Name
 ;; Gets the name of the second body
 connection-name2

 ;; Connection -> Number
 ;; Gets the distance between the two bodies of the connectoin
 connection-distance

 ;; Name [Listof Name] -> #<procedure:Name>
 ;; Creates a system of connections
 system

 ;; System -> [Listof Name]
 ;; Gets the name of the system
 system-loname

 ;; ---------------------------- Questions --------------------------------------

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

 ;; Connection -> VOID
 ;; displays all lagrange points of a connection
 lagrange
 ;; ---------------------------- Conversions ------------------------------------

 ;; Number -> Number
 ;; Converts seconds to days
 seconds->days

 
 )

;;------------------------------ Definitions ------------------------------------

(struct posn [x y]
  #:transparent)

;; A Name is a name of a body, represented as an identifier

(struct body-struct [mass radius])

;; A Body is a (body Name Number Number)
(define-syntax body
  (syntax-parser
    [(_ name:id mass radius)
     #'(define name (body-struct mass radius))]))

(define-syntax body-mass
  (syntax-parser
    [(_ body)
     #'(body-struct-mass body)]))

(define-syntax body-radius
  (syntax-parser
    [(_ body)
     #'(body-struct-radius body)]))


(struct connection-struct [name1 name2 distance])

;; A Connection is a (struct Name Name Number)

(define-syntax connection
  (syntax-parser
    [(_ name:id name1 name2 distance)
     #'(define name (connection-struct name1 name2 distance))]))

(define-syntax connection-name1
  (syntax-parser
    [(_ connection)
     #'(connection-struct-name1 connection)]))

(define-syntax connection-name2
  (syntax-parser
    [(_ connection)
     #'(connection-struct-name2 connection)]))

(define-syntax connection-distance
  (syntax-parser
    [(_ connection)
     #'(connection-struct-distance connection)]))

(struct system-struct [loname])

;; A System is a (struct [Listof Name])

(define-syntax system
  (syntax-parser
    [(_ name:id l)
     #'(define name (system-struct l))]))

(define-syntax system-loname
  (syntax-parser
    [(_ system)
     #'(system-struct-loname system)]))

;;-------------------------------- Questions ------------------------------------

(define G 6.6740831e-11)

(define-syntax surface-gravity
  (syntax-parser
    [(_ body)
     #'(/ (* G (body-mass body)) (sqr (body-radius body)))]))

(define-syntax surface-gravity-out
  (syntax-parser
    [(_ body)
     #'`(/ ,(* G (body-mass body)) ,(sqr (body-radius body)))]))

(define-syntax escape-velocity
  (syntax-parser
    [(_ body)
     #'(sqrt (/ (* 2 G (body-mass body)) (body-radius body)))]))

(define-syntax kepler3-period
  (syntax-parser
    [(_ connect)
     #'(sqrt (/ (* 4 (sqr 3.14159265359) (expt (connection-distance connect) 3))
                (* G (max (body-mass (connection-name1 connect))
                          (body-mass (connection-name2 connect))))))]))

(define-syntax L1
  (syntax-parser
    [(_ connect)
     #'(local ((define M1 (max (body-mass (connection-name1 connect))
                               (body-mass (connection-name2 connect))))
               (define M2 (min (body-mass (connection-name1 connect))
                               (body-mass (connection-name2 connect))))
               (define α (/ M2 (+ M1 M2)))
               (define x-coor (* (connection-distance connect)
                                 (- 1 (expt (/ α 3) (/ 1 3)))))
               (define y-coor 0))
         (posn x-coor y-coor))]))

(define-syntax L2
  (syntax-parser
    [(_ connect)
     #'(local ((define M1 (max (body-mass (connection-name1 connect))
                               (body-mass (connection-name2 connect))))
               (define M2 (min (body-mass (connection-name1 connect))
                               (body-mass (connection-name2 connect))))
               (define α (/ M2 (+ M1 M2)))
               (define x-coor (* (connection-distance connect)
                                 (+ 1 (expt (/ α 3) (/ 1 3)))))
               (define y-coor 0))
         (posn x-coor y-coor))]))

(define-syntax L3
  (syntax-parser
    [(_ connect)
     #'(local ((define M1 (max (body-mass (connection-name1 connect))
                               (body-mass (connection-name2 connect))))
               (define M2 (min (body-mass (connection-name1 connect))
                               (body-mass (connection-name2 connect))))
               (define α (/ M2 (+ M1 M2)))
               (define x-coor (* (* -1 (connection-distance connect))
                                 (+ 1 (* (/ 5 12) α))))
               (define y-coor 0))
         (posn x-coor y-coor))]))

(define-syntax L4
  (syntax-parser
    [(_ connect)
     #'(local ((define M1 (max (body-mass (connection-name1 connect))
                               (body-mass (connection-name2 connect))))
               (define M2 (min (body-mass (connection-name1 connect))
                               (body-mass (connection-name2 connect))))
               (define R (connection-distance connect))
               (define x-coor (* (/ R 2)
                                 (/ (- M1 M2)
                                    (+ M1 M2))))
               (define y-coor (* (/ (sqrt 3) 2) R)))
         (posn x-coor y-coor))]))

(define-syntax L5
  (syntax-parser
    [(_ connect)
     #'(local ((define M1 (max (body-mass (connection-name1 connect))
                               (body-mass (connection-name2 connect))))
               (define M2 (min (body-mass (connection-name1 connect))
                               (body-mass (connection-name2 connect))))
               (define R (connection-distance connect))
               (define x-coor (* (/ R 2)
                                 (/ (- M1 M2)
                                    (+ M1 M2))))
               (define y-coor (* (* -1 (/ (sqrt 3) 2)) R)))
         (posn x-coor y-coor))]))

(define-syntax lagrange
  (syntax-parser
    [(_ connect)
     #'(make-matrix 5 5
                    `(L1: x: ,(posn-x (L1 connect)) y: ,(posn-y (L1 connect))
                      L2: x: ,(posn-x (L2 connect)) y: ,(posn-y (L2 connect))
                      L3: x: ,(posn-x (L3 connect)) y: ,(posn-y (L3 connect))
                      L4: x: ,(posn-x (L4 connect)) y: ,(posn-y (L4 connect))
                      L5: x: ,(posn-x (L5 connect)) y: ,(posn-y (L5 connect))))]))

;;-------------------------------- Conversions ----------------------------------

(define-syntax seconds->days
  (syntax-parser
    [(_ n)
     #'(/ (/ (/ n 60) 60) 24)]))

