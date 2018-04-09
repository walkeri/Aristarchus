#lang racket

(require (for-syntax syntax/parse)
         "webpage.rkt"
         "latex.rkt")

(define-syntax (formula stx)
  (syntax-parse stx
    [(_ name (arg ...) assoc-list form)
     #'(define name (λ (arg ... #:mode [output 'answer])
                      (cond
                        [(symbol=? output 'answer) (answer assoc-list form)]
                        [(symbol=? output 'steps) (steps name assoc-list form)])))]))

(define-for-syntax (answer association formula)
  #'(let association
      formula))


(define-for-syntax (subst-nums association formula)
  #'(let association (#%latex formula)))

(define-for-syntax (subst-vars association formula)
  #'(apply-as-symbols association (#%latex formula)))

(define-syntax apply-as-symbols
  (syntax-parser
    [(_ ((a:id b)) body)
     #'((lambda (a) body) 'a)]
    [(_ ((a:id b) e ...) body)
     #'((lambda (a) (apply-as-symbols (e ...) body)) 'a)]))

(define-syntax (steps stx)
  (syntax-parse stx
    [(_ name association formula)
     #'(begin-for-syntax
         (define form-name name)
         (define no-sub (subst-vars #'association #'formula))
         (define sub (subst-nums #'association #'formula))
         (define ans (answer #'association #'formula))
         (generate-webpage form-name sub (syntax->list association) sub ans))]))

(steps "surface-gravity" ((M 4) (G 2)) (/ M G))


(define x (subst-vars ((M 4) (G 2)) (/ M G)))
x
