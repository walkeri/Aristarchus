#lang racket

(require (for-syntax syntax/parse))

(provide

 (rename-out [body-struct-mass body-mass]
             [body-struct-radius body-radius]
             [connection-struct-name1 connection-name1]
             [connection-struct-name2 connection-name2]
             [connection-struct-distance connection-distance]
             [system-struct-loname system-loname])
 
 ;; Name Number Number -> VOID
 ;; Creates a body with a mass and radius
 body
 
 ;; Name Name Name Number -> VOID
 ;; Creates a connection between two bodies
 connection

 ;; Name [Listof Name] -> VOID
 ;; Creates a system of connections
 system
 
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

#;(define-syntax body-mass
    (syntax-parser
      [(_ body)
       #'(body-struct-mass body)]))

#;(define-syntax body-radius
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
