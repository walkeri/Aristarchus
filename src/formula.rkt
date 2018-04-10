#lang racket

(require (for-syntax syntax/parse)
         "webpage.rkt"
         "latex.rkt")

(provide 
 formula)

;; A Name is an identifier

;; arg is an identifier

;; An Association list is a list of pairs where the first of each pair is the name of the variable
;; and the second is the value of the variable

;; (Name arg ...) Association-List MSE
;; defines a function that produces either an answer or a list of steps to get to that answer
(define-syntax (formula stx)
  (syntax-parse stx
    [(_ (name arg ...) assoc-list form)
     #'(define name (λ (arg ... #:mode [output 'answer])
                      (cond
                        [(symbol=? output 'answer) (answer assoc-list form)]
                        [(symbol=? output 'steps) (steps 'name assoc-list form)]
                        [else (error "unrecognized mode:" output)])))]))

;; An answer is one of:
;; - Number
;; - Posn

;; Association-List MSE -> Answer
;; Gets the answer to the formula from an association list
(define-syntax answer
  (syntax-parser
    [(_ association formula)
     #'(let association
         formula)]))

;; Association-List MSE -> String
;; Returns the latex form of the formula with its values substituted in
(define-syntax subst-vals
  (syntax-parser
    [(_ association formula)
     #'(let association (#%latex formula))]))

;; Association-List MSE -> String
;; Returns the latex form of the formula without its values substituted in
(define-syntax subst-vars
  (syntax-parser
    [(_ association formula)
     #'(apply-as-symbols association (#%latex formula))])) 

;; Association-List MSE -> Any
;; Given an Association-List, bind each identifier to its quoted form in the body
(define-syntax apply-as-symbols
  (syntax-parser
    [(_ ((a:id b)) body)
     #'((lambda (a) body) 'a)]
    [(_ ((a:id b) e ...) body)
     #'((lambda (a) (apply-as-symbols (e ...) body)) 'a)]))

;; Association-List -> Association-List
;; evaluates the second of all the pairs in an Association-List
(define-syntax quote-firsts
  (syntax-parser
    [(_  ((a b)))
     #'`((a ,b))]
    [(_  ((a b) e ...))
     #'`((a ,b) ,@(quote-firsts (e ...)))]))

;; Name Assocation MSE
;; Returns the generated webpage for a formula
(define-syntax (steps stx)
  (syntax-parse stx
    [(_ name association formula)
     #`(generate-webpage name
                         (subst-vars association formula)
                         (quote-firsts association)
                         (subst-vals association formula)
                         (answer association formula))]))