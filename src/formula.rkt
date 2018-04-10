#lang racket

(require (for-syntax syntax/parse)
         "webpage.rkt"
         "latex.rkt")

(provide

 ;posn posn-x posn-y
 
 formula)

(struct posn (x y) #:transparent)

(define-syntax (formula stx)
  (syntax-parse stx
    [(_ (name arg ...) assoc-list form)
     #'(define name (λ (arg ... #:mode [output 'answer])
                      (cond
                        [(symbol=? output 'answer) (answer assoc-list form)]
                        [(symbol=? output 'steps) (steps 'name assoc-list form)]
                        [else (error "unrecognized mode:" output)])))]))

(define-syntax answer
  (syntax-parser
    [(_ association formula)
     #'(let association
         formula)]))

(define-syntax subst-vals
  (syntax-parser
    [(_ association formula)
     #'(let association (#%latex formula))]))

(define-syntax subst-vars
  (syntax-parser
    [(_ association formula)
     #'(apply-as-symbols association (#%latex formula))])) 

(define-syntax apply-as-symbols
  (syntax-parser
    [(_ ((a:id b)) body)
     #'((lambda (a) body) 'a)]
    [(_ ((a:id b) e ...) body)
     #'((lambda (a) (apply-as-symbols (e ...) body)) 'a)]))

(define-syntax quote-firsts
  (syntax-parser
    [(_  ((a b)))
     #'`((a ,b))]
    [(_  ((a b) e ...))
     #'`((a ,b) ,@(quote-firsts (e ...)))]))

(define-syntax (steps stx)
  (syntax-parse stx
    [(_ name association formula)
     #`(generate-webpage "../src/output.html"
                         name
                         (subst-vars association formula)
                         (quote-firsts association)
                         (subst-vals association formula)
                         (answer association formula))]))


;(apply-as-symbols ((r 100)) (#%latex (posn (* r (- 1 (expt (/ 55 3) (/ 1 3)))) 10)))