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

(define-syntax (answer stx)
  (syntax-parse stx
    [(_ association formula)
     #'(let association
         formula)]))

(define-syntax (steps stx)
  (syntax-parse stx
    [(_ name association formula)
     #:with form-name #'name
     #:with no-sub #'(#%latex formula)
     #:with sub #'(#%latex formula)
     #:with ans #'formula
     #'(let association (generate-webpage form-name no-sub association sub ans))]))


(steps "surface-gravity" ((G 2) (M 3)) (/ G M))

     #;(begin
         (define form-name (syntax->datum #'name))
         (define l-vars (let association (#%latex formula)))
         (define lop association)
         (define l-sub (let association (#%latex formula)))
         (define ans (answer association formula))
         (generate-webpage form-name l-vars lop l-sub ans))

#;(define-syntax (subst stx)
    (syntax-parse stx
      [(_ name association formula)
       #'(let association
           (#%latex formula))]))

(define-syntax (bind-to-symbols stx)
  (syntax-parse stx
    [(_ (a b))
     #'(a (syntax->datum a))]
    [(_ (a b) e ...)
     #'(cons (a (syntax->datum a)) (bind-to-symbols e ...))]))
    

;; For example:
#;(formula sg body
         ((G G)
          (M (body-mass body))
          (R (body-radius body)))
         (/ (* G M) (expt R 2)))