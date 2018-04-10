#lang racket

(require (for-syntax syntax/parse)
         ;(prefix-in un: racket)
         htdp/matrix
         "formula.rkt")

(provide

 #%module-begin
 #%datum
 #%app
 #%top-interaction
 quote

 ;;------------------------------ Definitions ----------------------------------

 ;; Number Number -> #<procedure:posn>
 ;posn
 
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
 ;; Gets the distance between the two bodies of the connection
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
#|
 ;; Connection -> Number
 ;; generates a webpage for L1
 L1-out

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

|#
 ;; ---------------------------- Conversions ------------------------------------

 ;; Number -> Number
 ;; Converts seconds to days
 seconds->days

 ;; Number -> Number
 ;; Converts days to seconds
 days->seconds

 ;; Number -> Number
 ;; Converts hours to seconds
 hours->seconds

 ;; Number -> Number
 ;; Converts seconds to hours
 seconds->hours

 ;; Number -> Number
 ;; Converts minutes to seconds
 minutes->seconds

 ;; Number -> Number
 ;; Converts seconds to minutes
 seconds->minutes

 ;; Number -> Number
 ;; Converts kilometers to meters
 kilometers->meters

 ;; Number -> Number
 ;; Converts meters to kilometers
 meters->kilometers

 ;; Number -> Number
 ;; Converts meters to centimeters
 meters->centimeters

 ;; Number -> Number
 ;; Converts centimeters to meters
 centimeters->meters
 
 )

;;------------------------------ Definitions ------------------------------------


#;
(struct posn [x y]
    #:transparent)

;; A WebPage is an outputted webpage to the user's default browser

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

(formula (surface-gravity body)
         ((G G) (M_b (body-mass body)) (r (body-radius body)))
         (/ (* G M_b) (expt r 2)))

(formula (escape-velocity body)
         ((G G) (M_b (body-mass body)) (r (body-radius body)))
         (sqrt (/ (* 2 G M_b) r)))

(formula (kepler3-period conn)
         ((G G) (M (max (body-mass (connection-name1 conn)) (body-mass (connection-name2 conn)))) (r (connection-distance conn)) (\\pi pi))
         (sqrt (/ (* 4 (expt \\pi 2) (expt r 3))
                  (* G M))))

(struct posn (x y) #:transparent)

(formula (L1 conn)
          ((r (connection-distance conn))
           (alpha (/ (min (body-mass (connection-name1 conn)) (body-mass (connection-name2 conn)))
                       (+ (max (body-mass (connection-name1 conn)) (body-mass (connection-name2 conn)))
                          (min (body-mass (connection-name1 conn)) (body-mass (connection-name2 conn)))))))
          (/ (* r (- 1 (expt (/ alpha 3) (/ 1 3)))) 10))

#|



#;



#;
(formula L1-internal-sub (\\alpha R)
         (posn (* ,R
                  (add-brackets (- 1 (expt (add-parens (/ ,\\alpha 3)) (/ 1 3)))))
               0))

(define (L1 connect)
  (define M1 (max (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define M2 (min (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define α (/ M2 (+ M1 M2)))
  (define R (connection-distance connect))
  (define x-coor (* R (- 1 (expt (/ α 3) (/ 1 3)))))
  (define y-coor 0)
  (posn x-coor y-coor))

(define (L1-out connect)
  (define M1 (max (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define M2 (min (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define α (/ M2 (+ M1 M2)))
  (define R (connection-distance connect))
  (define name "L1 point")
  ;(define ans-name "(x, y)")
  (define l-vars (L1-internal-latex α R))
  (define lolt (list (list 'M_1 M1 "kilograms")
                     (list 'M_2 M2 "kilograms")
                     (list '\\alpha α "")
                     (list 'R R "meters")))
  (define l-sub (L1-internal-sub-latex α R))
  (define ans (L1 connect))
  (define ans-unit "meters")
  (generate-webpage name l-vars lolt l-sub ans ans-unit))

#;
(formula L2-internal (\\alpha R)
         (posn (* R
                  (add-brackets (- 1 (expt (add-parens (/ \\alpha 3)) (/ 1 3)))))
               0))

#;
(formula L2-internal-sub (\\alpha R)
         (posn (* ,R
                  (add-brackets (- 1 (expt (add-parens (/ ,\\alpha 3)) (/ 1 3)))))
               0))

(define (L2 connect)
  (define M1 (max (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define M2 (min (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define α (/ M2 (+ M1 M2)))
  (define R (connection-distance connect))
  (define x-coor (* R (+ 1 (expt (/ α 3) (/ 1 3)))))
  (define y-coor 0)
  (posn x-coor y-coor))

(define (L2-out connect)
  (define M1 (max (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define M2 (min (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define α (/ M2 (+ M1 M2)))
  (define R (connection-distance connect))
  (define name "L2 point")
  ;(define ans-name "(x, y)")
  (define l-vars (L2-internal-latex α R))
  (define lolt (list (list 'M_1 M1 "kilograms")
                     (list 'M_2 M2 "kilograms")
                     (list '\\alpha α "")
                     (list 'R R "meters")))
  (define l-sub (L2-internal-sub-latex α R))
  (define ans (L2 connect))
  (define ans-unit "meters")
  (generate-webpage name l-vars lolt l-sub ans ans-unit))

#;
(formula L3-internal (\\alpha R)
         (posn (* (* -1 R)
                  (add-brackets (+ 1 (* (add-parens (/ 5 12)) \\alpha))))
               0))

#;
(formula L3-internal-sub (\\alpha R)
         (posn (* (* -1 ,R)
                  (add-brackets (+ 1 (* (add-parens (/ 5 12)) ,\\alpha))))
               0))

(define (L3 connect)
  (define M1 (max (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define M2 (min (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define α (/ M2 (+ M1 M2)))
  (define R (connection-distance connect))
  (define x-coor (* (* -1 R)
                    (+ 1 (* (/ 5 12) α))))
  (define y-coor 0)
  (posn x-coor y-coor))

(define (L3-out connect)
  (define M1 (max (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define M2 (min (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define α (/ M2 (+ M1 M2)))
  (define R (connection-distance connect))
  (define name "L3 point")
  ;(define ans-name "(x, y)")
  (define l-vars (L3-internal-latex α R))
  (define lolt (list (list 'M_1 M1 "kilograms")
                     (list 'M_2 M2 "kilograms")
                     (list '\\alpha α "")
                     (list 'R R "meters")))
  (define l-sub (L3-internal-sub-latex α R))
  (define ans (L3 connect))
  (define ans-unit "meters")
  (generate-webpage name l-vars lolt l-sub ans ans-unit))

#;
(formula L4-internal (M1 M2 R)
         (posn (* (/ R 2)
                  (add-parens (/ (- M1 M2)
                                 (+ M1 M2))))
               (* (/ (sqrt 3) 2) R)))

#;
(formula L4-internal-sub (M1 M2 R)
         (posn (* (/ ,R 2)
                  (add-parens (/ (- ,M1 ,M2)
                                 (+ ,M1 ,M2))))
               (* (/ (sqrt 3) 2) ,R)))

(define (L4 connect)
  (define M1 (max (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define M2 (min (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define R (connection-distance connect))
  (define x-coor (* (/ R 2)
                    (/ (- M1 M2)
                       (+ M1 M2))))
  (define y-coor (* (/ (sqrt 3) 2) R))
  (posn x-coor y-coor))

(define (L4-out connect)
  (define M1 (max (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define M2 (min (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define R (connection-distance connect))
  (define name "L4 point")
  ;(define ans-name "(x, y)")
  (define l-vars (L4-internal-latex M1 M2 R))
  (define lolt (list (list 'M_1 M1 "kilograms")
                     (list 'M_2 M2 "kilograms")
                     (list 'R R "meters")))
  (define l-sub (L4-internal-sub-latex M1 M2 R))
  (define ans (L4 connect))
  (define ans-unit "meters")
  (generate-webpage name l-vars lolt l-sub ans ans-unit))

#;
(formula L5-internal (M1 M2 R)
         (posn (* (/ R 2)
                  (add-parens (/ (- M1 M2)
                                 (+ M1 M2))))
               (* (* -1 (/ (sqrt 3) 2)) R)))

#;
(formula L5-internal-sub (M1 M2 R)
         (posn (* (/ ,R 2)
                  (add-parens (/ (- ,M1 ,M2)
                                 (+ ,M1 ,M2))))
               (* (* -1 (/ (sqrt 3) 2)) ,R)))

(define (L5 connect)
  (define M1 (max (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define M2 (min (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define R (connection-distance connect))
  (define x-coor (* (/ R 2)
                    (/ (- M1 M2)
                       (+ M1 M2))))
  (define y-coor (* (* -1 (/ (sqrt 3) 2)) R))
  (posn x-coor y-coor))

(define (L5-out connect)
  (define M1 (max (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define M2 (min (body-mass (connection-name1 connect))
                  (body-mass (connection-name2 connect))))
  (define R (connection-distance connect))
  (define name "L5 point")
  ;(define ans-name "(x, y)")
  (define l-vars (L5-internal-latex M1 M2 R))
  (define lolt (list (list 'M_1 M1 "kilograms")
                     (list 'M_2 M2 "kilograms")
                     (list 'R R "meters")))
  (define l-sub (L5-internal-sub-latex M1 M2 R))
  (define ans (L5 connect))
  (define ans-unit "meters")
  (generate-webpage name l-vars lolt l-sub ans ans-unit))

(define (lagrange connect)
  (make-matrix 5 5
               `(L1: x: ,(posn-x (L1 connect)) y: ,(posn-y (L1 connect))
                     L2: x: ,(posn-x (L2 connect)) y: ,(posn-y (L2 connect))
                     L3: x: ,(posn-x (L3 connect)) y: ,(posn-y (L3 connect))
                     L4: x: ,(posn-x (L4 connect)) y: ,(posn-y (L4 connect))
                     L5: x: ,(posn-x (L5 connect)) y: ,(posn-y (L5 connect)))))

|#
;;-------------------------------- Conversions ----------------------------------

(define (seconds->days n)
  (/ (/ (/ n 60) 60) 24))

(define (days->seconds n)
  (* (* (* n 24) 60) 60))

(define (hours->seconds n)
  (* (* n 60) 60))

(define (seconds->hours n)
  (/ (/ n 60) 60))

(define (minutes->seconds n)
  (* n 60))

(define (seconds->minutes n)
  (/ n 60))

(define (kilometers->meters n)
  (* 1000 n))

(define (meters->kilometers n)
  (/ n 1000))

(define (meters->centimeters n)
  (* 100 n))

(define (centimeters->meters n)
  (/ n 100))
