#lang racket

(require (for-syntax syntax/parse)
         (prefix-in un: racket))

(provide

 (all-from-out racket)
 
 ;; Name Number Number -> #<procedure:Name>
 body

 ;; Body -> Number
 body-mass

 ;; Body -> Number
 body-radius
 
 
 ;; Name Name Name Number -> #<procedure:Name>
 connection

 ;; Connection -> Name
 connection-name1

 ;; Connection -> Name
 connection-name2

 ;; Connection -> Number
 connection-distance

 ;; Name [Listof Name] -> #<procedure:Name>
 system

 ;; System -> [Listof Name]
 system-loname
 )


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