#lang racket

(require (for-syntax syntax/parse))

(provide

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

 ;; A Posn is a (posn Number Number)
 
 ;; Number Number -> #<procedure:posn>
 ;; Structure representing a coordinate pair
 posn

 ;; Posn -> Number
 ;; Gets the x-coordinate of a posn
 posn-x
 
 ;; Posn -> Number
 ;; Gets the y-coordinate of a posn
 posn-y

 ;; Any -> Boolean
 ;; is this a posn?
 posn?)

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
