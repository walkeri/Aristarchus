#lang racket

(require (for-syntax syntax/parse
                     racket/syntax))

(provide

 ;; Name [Listof Name] Math-s-expression
 ;; A definitional form that binds name to the function made by the s-expression,
 ;; but also binds another function that takes Latexpressions and returns a string
 ;; of LaTeX.
 formula)

(define-syntax (formula stx)
  (syntax-parse stx
    [(_ name args body)
     #`(begin
         (define name (lambda args (eval `body (current-namespace))))
         (define #,(format-id stx "~a-latex" #'name) (lambda args (latexpression->string (math-s-exp->latexpression `body)))))]))

;; For example:
(formula ev (grav-const mass-body radius)
         (sqrt (/ (* 2 ,grav-const ,mass-body) ,radius)))

;; A Latexpression is one of:
;; - String
;; - Symbol
;; - Number
;; - (list 'keep-flat [Listof Latexpression])
;; - (list Symbol [Listof Latexpression] ...)

;; Corresponds to someting like: \frac{G \times M_b}{r^2}
(define example-latexpression
  '(frac {G (times) M_b} {r^2}))

;; Latexpression -> String
;; The string representation of a Latexpression
(define (latexpression->string lx)
  (cond
    [(string? lx) lx]
    [(symbol? lx) (symbol->string lx)]
    [(number? lx) (number->string lx)]
    [(and (symbol? (first lx)) (symbol=? (first lx) 'keep-flat))
     (latexpression-list->string (second lx))]
    [else (string-append "\\"
                         (symbol->string (first lx))
                         (latexpression-list-list->string (rest lx)))]))

;; ([Listof Latexpression] ...) -> String
(define (latexpression-list-list->string l)
  (foldr string-append "" (map wrap-in-braces (map latexpression-list->string l))))

;; [Listof Latexpression] -> String
(define (latexpression-list->string l)
  (foldr (lambda (x y) (string-append x " " y)) "" (map latexpression->string l)))

;; String -> String
;; Adds braces around the string
;; ex: (wrap-in-braces "hello") -> "{hello}"
(define (wrap-in-braces s)
  (string-append "{" s "}"))

;; A Math-s-expression is one of:
;; - (+ Math-body Math-body ...)
;; - (- Math-body ...)
;; - (* Math-body Math-body ...)
;; - (/ Math-body Math-body)
;; - (sqrt Math-body)
;; - (sqr Math-body)
;; - (expt Math-body Math-body)

;; A Math-body is one of:
;; - Math-s-expression
;; - String
;; - Symbol
;; - Number

(define (math-body->latexpression m)
  (cond
    [(string? m) m]
    [(symbol? m) m]
    [(number? m) m]
    [else (math-s-exp->latexpression m)]))

(define (math-s-exp->latexpression m)
  (match m
    [`(/ ,a ,b) `(frac (,(math-body->latexpression a)) (,(math-body->latexpression b)))]
    [`(+ ,a ,b) `(keep-flat (,(math-body->latexpression a) +  ,(math-body->latexpression b)))]
    [`(+ ,a ,b ...) `(keep-flat (,(math-body->latexpression a) + (,@(math-s-exp->latexpression `(+ ,@b)))))]
    [`(* ,a ,b) `(keep-flat (,(math-body->latexpression a) (times)  ,(math-body->latexpression b)))]
    [`(* ,a ,b ...) `(keep-flat (,(math-body->latexpression a) (times) (,@(math-s-exp->latexpression `(* ,@b)))))]
    [`(- ,a ,b) `(keep-flat (,(math-body->latexpression a) -  ,(math-body->latexpression b)))]
    [`(- ,a ,b ...) `(keep-flat (,(math-body->latexpression a) - (,@(math-s-exp->latexpression `(- ,@b)))))]
    [`(sqrt ,a) `(sqrt (,(math-body->latexpression a)))]
    [`(sqr ,a) `(keep-flat (,(math-body->latexpression a) "^2"))]
    [`(expt ,a ,e) `(keep-flat (,(math-body->latexpression a) "^{" ,(math-body->latexpression e) "}"))]))


#|
(define x '(/ 5 (+ 1 (+ 8 5 (expt 6 98) 6 8 7))))
(math-s-exp->latexpression x)
(latexpression->string (math-s-exp->latexpression x))|#