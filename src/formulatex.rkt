#lang racket

(require (for-syntax syntax/parse
                     racket/syntax))

(provide

 ;; Name [Listof Name] Math-s-expression
 ;; A definitional form that binds name to the function made by the s-expression,
 ;; but also binds another function that takes Latexpressions and returns a string
 ;; of LaTeX.
 formula)

#|
(formula surface-gravity (body)
         `((G ,G)
           (M ,(body-mass body))
           (R ,(body-radius body)))
         (/ (* G M) (expt R 2)))
|#

(define-syntax (formula stx)
  (syntax-parse stx
    [(_ name args assoc-list form)
     #'(define name (λ (args #:mode [output 'answer])
                      (cond
                        [(symbol=? output 'answer) (answer assoc-list form)]
                        [(symbol=? output 'steps) (steps name assoc-list form)])))]))

(define-syntax (answer stx)
  (syntax-parse stx
    [(_ association formula)
     #'(let association
       formula)]))

#;(define-syntax (steps stx)
  (syntax-parse stx
    [(_ name association formula)
     #'(begin
         (define form-name (symbol->string (syntax->datum name)))
         (define l-vars (latexpression->string (math-s-exp->latexpression ))))]))

;; For example:
#;(formula surface-gravity (body)
         `((G ,G G)
           (M ,(body-mass body) M_b)
           (R ,(body-radius body)) R_b)
         (/ (* G M) (expt R 2)))
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

(define-for-syntax (add-parens x)
  `(|(| ,x |)|))

(define-for-syntax (add-brackets x)
  `(|[| ,x |]|))

(define-syntax (#%latex stx)
  (syntax-parse stx
    [(_ (/ a b)) #'`("\\frac{" ,(#%latex a) "}{" ,(#%latex b) "}")]
    [(_ (+ a b)) #'`(,(#%latex a) " + " ,(#%latex b))]
    #;[(_ + a b ...) ...]
    [(_ (* a b)) #'`(,(#%latex a) " \\times " ,(#%latex b))]
    #;[(_ * a b ...) ...]
    [(_ (- a b)) #'`(,(#%latex a) " - " ,(#%latex b))]
    #;[(_ - a b ...) ...]
    [(_ (sqrt a)) #'`("\\sqrt{" ,(#%latex a) "}")]
    [(_ (expt a e)) #'`(,(#%latex a) " ^{ " ,(#%latex e) "}")]
    [(_ (posn x y)) #'`("(" ,(#%latex x) "," ,(#%latex y) ")")]
    [(_ (add-parens x)) #'`("(" ,(#%latex x) ")")]
    [(_ (add-brackets x)) #'`("[" ,(#%latex x) "]")]
    [(_ a) #'(convert-to-string a)]))

(define (convert-to-string x)
  (cond
    [(symbol? x) (symbol->string x)]
    [(number? x) (number->string x)]))
    


(define (math-s-exp->latexpression m)
  (match m
    [`(/ ,a ,b)
     `(frac (,(math-body->latexpression a)) (,(math-body->latexpression b)))]
    [`(+ ,a ,b)
     `(keep-flat (,(math-body->latexpression a) +  ,(math-body->latexpression b)))]
    [`(+ ,a ,b ...)
     `(keep-flat (,(math-body->latexpression a) + (,@(math-s-exp->latexpression `(+ ,@b)))))]
    [`(* ,a ,b)
     `(keep-flat (,(math-body->latexpression a) (times)  ,(math-body->latexpression b)))]
    [`(* ,a ,b ...)
     `(keep-flat (,(math-body->latexpression a) (times) (,@(math-s-exp->latexpression `(* ,@b)))))]
    [`(- ,a ,b)
     `(keep-flat (,(math-body->latexpression a) -  ,(math-body->latexpression b)))]
    [`(- ,a ,b ...)
     `(keep-flat (,(math-body->latexpression a) - (,@(math-s-exp->latexpression `(- ,@b)))))]
    [`(sqrt ,a)
     `(sqrt (,(math-body->latexpression a)))]
    [`(sqr ,a)
     `(keep-flat (,(math-body->latexpression a) "^2"))]
    [`(expt ,a ,e)
     `(keep-flat (,(math-body->latexpression a) "^{" ,(math-body->latexpression e) "}"))]
    [`(posn ,x ,y)
     `(keep-flat ("(" ,(math-body->latexpression x) "," ,(math-body->latexpression y) ")"))]
    [`(|(| ,x |)|)
     `(keep-flat ("(" ,(math-body->latexpression x) ")"))]
    [`(add-parens ,x)
     `(keep-flat ("(" ,(math-body->latexpression x) ")"))]
    [`(add-brackets ,x)
     `(keep-flat ("[" ,(math-body->latexpression x) "]"))]))


#|
(define x '(/ 5 (+ 1 (+ 8 5 (expt 6 98) 6 8 7))))
(math-s-exp->latexpression x)
(latexpression->string (math-s-exp->latexpression x))|#