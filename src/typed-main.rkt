#lang turnstile/quicklang

(require (prefix-in m: "untyped-main.rkt")
         )

(provide

 define
 quote
 assign-unit
 body-type
 connection-type
 system-type
 distance
 mass
 vel
 accel
 number
 time
 coordinate
 name
 Listof
 void

 (rename-out [typed-datum #%datum]
             [typed-app #%app])
 
 (typed-out [[m:body-struct : (-> mass distance body-type)]  body-struct]
            [[m:body-struct-mass : (-> body-type mass)]  body-struct-mass]
            [[m:body-struct-radius : (-> body-type distance)] body-struct-radius]
            [[m:connection-struct : (-> name name distance connection-type)] connection-struct]
            [[m:connection-struct-name1 : (-> connection-type name)] connection-struct-name1]
            [[m:connection-struct-name2 : (-> connection-type name)] connection-struct-name2]
            [[m:connection-struct-distance
              : (-> connection-type distance)] connection-struct-distance]
            [[m:system-struct : (-> [Listof name] system-type)] system-struct]
            [[m:system-struct-loname : (-> system-type [Listof name])] system-struct-loname])
             
 

 (rename-out [typed-body body]
             [typed-body-mass body-mass]
             [typed-body-radius body-radius]
             [typed-connection connection]
             [typed-connection-name1 connection-name1]
             [typed-connection-name2 connection-name2]
             [typed-connection-distance connection-distance]
             [typed-system system]
             [typed-surface-gravity surface-gravity]
             [typed-escape-velocity escape-velocity]
             [typed-kepler3-period kepler3-period]
             [typed-L1 L1]
             [typed-L2 L2]
             [typed-L3 L3]
             [typed-L4 L4]
             [typed-L5 L5]
             [typed-lagrange lagrange])

 )

(define-base-types
  body-type
  connection-type
  system-type
  distance
  mass
  vel
  accel
  number
  time
  coordinate
  name
  void)

(define-type-constructor -> #:arity > 0)
(define-type-constructor Listof #:arity > 0)

(define-typed-syntax assign-unit
  [(_ e t) ≫
           [⊢ e ≫ e- ⇐ t]
           ---------------
           [⊢ e- ⇒ t]])



(define-typed-syntax typed-datum
  [(_ . x)≫
   ---------------
   [#:error "could infer the unit"]]
  [(_ . x)⇐ τ ≫
   ---------------
   [⊢ (#%datum- . x)]])

(define-typed-syntax typed-app
  [(_ f e ...)≫
   [⊢ f ≫ f- ⇒ (~-> τin ... τout)]
   #:fail-unless (= (stx-length #'(e ...)) (stx-length #'(τin ...)))
   "number of given arguments incorrect"
   [⊢ e ≫ e- ⇐ τin] ...
   ---------------
   [⊢ (#%app f- e- ...) ⇒ τout]]) 

(define-typed-syntax typed-body
  [(_ name:id mass radius)≫
   ------------------------
   [≻ (begin
        (define-syntax name
          (make-variable-like-transformer
           (assign-type #'internal-name #'body-type)))
        (m:body internal-name mass radius))]])

(define-typed-syntax typed-body-mass
  [(_ body)≫
   [⊢ body ≫ body- ⇐ body-type]    
   ------------------------
   [⊢ (m:body-mass body-) ⇒ mass]])

(define-typed-syntax typed-body-radius
  [(_ body)≫
   [⊢ body ≫ body- ⇐ body-type]    
   ------------------------
   [⊢ (m:body-radius body-) ⇒ distance]])

(define-typed-syntax typed-connection
  [(_ name:id name1 name2 distance)≫
   ------------------------
   [≻ (begin
        (define-syntax name
          (make-variable-like-transformer
           (assign-type #'internal-name #'connection-type)))
        (m:connection internal-name name1 name2 distance))]])

(define-typed-syntax typed-connection-name1
  [(_ connection)≫
   [⊢ connection ≫ connection- ⇐ connection-type]    
   ------------------------
   [⊢ (m:connection-name1 connection-) ⇒ name]])

(define-typed-syntax typed-connection-name2
  [(_ connection)≫
   [⊢ connection ≫ connection- ⇐ connection-type]    
   ------------------------
   [⊢ (m:connection-name2 connection-) ⇒ name]])

(define-typed-syntax typed-connection-distance
  [(_ connection)≫
   [⊢ connection ≫ connection- ⇐ connection-type]    
   ------------------------
   [⊢ (m:connection-distance connection-) ⇒ distance]])

(define-typed-syntax typed-system
  [(_ name:id l)≫
   ------------------------
   [≻ (begin
        (define-syntax name
          (make-variable-like-transformer
           (assign-type #'internal-name #'system-type)))
        (m:system internal-name l))]])

(define-typed-syntax typed-system-loname
  [(_ system)≫
   [⊢ system ≫ system- ⇐ system-type]    
   ------------------------
   [⊢ (m:system-loname system-) ⇒ [Listof name]]])


(define-typed-syntax typed-surface-gravity
  [(_ body)≫
   [⊢ body ≫ body- ⇐ body-type]    
   ------------------------
   [⊢ (m:surface-gravity body-) ⇒ accel]])

(define-typed-syntax typed-escape-velocity
  [(_ body) ≫
   [⊢ body ≫ body- ⇐ body-type]
   ------------------------
   [⊢ (m:escape-velocity body-) ⇒ vel]])

(define-typed-syntax typed-kepler3-period
  [(_ connection) ≫
   [⊢ connection ≫ connection- ⇐ connection-type]
   ------------------------
   [⊢ (m:kepler3-period connection-) ⇒ time]])

(define-typed-syntax typed-L1
  [(_ connection) ≫
   [⊢ connection ≫ connection- ⇐ connection-type]
   ------------------------
   [⊢ (m:L1 connection-) ⇒ coordinate]])

(define-typed-syntax typed-L2
  [(_ connection) ≫
   [⊢ connection ≫ connection- ⇐ connection-type]
   ------------------------
   [⊢ (m:L2 connection-) ⇒ coordinate]])

(define-typed-syntax typed-L3
  [(_ connection) ≫
   [⊢ connection ≫ connection- ⇐ connection-type]
   ------------------------
   [⊢ (m:L3 connection-) ⇒ coordinate]])


(define-typed-syntax typed-L4
  [(_ connection) ≫
   [⊢ connection ≫ connection- ⇐ connection-type]
   ------------------------
   [⊢ (m:L4 connection-) ⇒ coordinate]])

(define-typed-syntax typed-L5
  [(_ connection) ≫
   [⊢ connection ≫ connection- ⇐ connection-type]
   ------------------------
   [⊢ (m:L5 connection-) ⇒ coordinate]])

(define-typed-syntax typed-lagrange
  [(_ connection) ≫
   [⊢ connection ≫ connection- ⇐ connection-type]
   ------------------------
   [⊢ (m:lagrange connection-) ⇒ void]])