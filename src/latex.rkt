#lang racket

(require (for-syntax syntax/parse)
         (prefix-in racket: racket))

(provide
 
 ;; (#%latex MSE) transforms the MSE into syntax that evaluates to the string representing it in LaTeX
 #%latex

 +

 /
 *
 expt
 sqrt)

;; An MSE (Math-s-expression) is one of:
;; - Identifier
;; - Symbol
;; - Number
;; - String
;; - (+ MSE MSE ...)
;; - (- MSE MSE ...)
;; - (* MSE MSE ...)
;; - (/ MSE MSE)
;; - (sqrt MSE)
;; - (expt MSE MSE)
;; - (posn MSE MSE)


(begin-for-syntax
  ;; gen-latex transforms the syntax
  (struct latex-expander [proc-id gen-latex]
    #:property prop:procedure
    (λ (le stx)
      (syntax-parse stx
       [(_ a ...) #`(#,(latex-expander-proc-id le) a ...)]))))

;; MSE -> String
;; Converts an MSE to its corresponding latex in string form.
(define-syntax (#%latex stx)
  (syntax-parse stx
    [(_ (f a ...))
     (define le
       (syntax-local-value #'f))
     ((latex-expander-gen-latex le) #'(a ...))]
    [(_ i:id) #'(convert-to-string i)]
    [(_ n:number) #'(convert-to-string n)]
    [(_ s:string) #'s]))


(define-syntax /
  (latex-expander
   #'racket:/
   (syntax-parser
     [(n d) #'(string-append "\\frac{" (#%latex n) "}{" (#%latex d) "}")])))

(define-syntax +
  (latex-expander
   #'racket:+
   (syntax-parser
     [(a b) #'(string-append (#%latex a) " + " (#%latex b))]
     [( a b ...) #'(string-append (#%latex a) " + " (#%latex (+ b ...)))])))

(define-syntax *
  (latex-expander
   #'racket:*
   (syntax-parser
     [(a b) #'(string-append (#%latex a) " \\times " (#%latex b))]
     [( a b ...) #'(string-append (#%latex a) " \\times " (#%latex (* b ...)))])))


(define-syntax -
  (latex-expander
   #'racket:-
   (syntax-parser
     [(a b) #'(string-append (#%latex a) " - " (#%latex b))]
     [(a b ...) #'(string-append (#%latex a) " - " (#%latex (- b ...)))])))

(define-syntax expt
  (latex-expander
   #'racket:expt
   (syntax-parser
     [((a ...) e) #'(string-append "\\left( " (#%latex (a ...))
                                                      " \\right)^{" (#%latex e) "}")] 
     [(a e) #'(string-append (#%latex a) "^{ " (#%latex e) " }")])))

(define-syntax sqrt
  (latex-expander
   #'racket:sqrt
   (syntax-parser
    [(a) #'(string-append "\\sqrt{" (#%latex a) "}")])))



#|(/ a b) -> (/ 5 6)
(/ a b) -> (/ 'a 'b)|#

#;([(_ ((~literal /) a b)) #'(string-append "\\frac{" (#%latex a) "}{" (#%latex b) "}")]
    [(_ ((~literal +) a b)) #'(string-append (#%latex a) " + " (#%latex b))]
    [(_ ((~literal +) a b ...)) #'(string-append (#%latex a) " + " (#%latex (+ b ...)))]
    [(_ ((~literal *) a b)) #'(string-append (#%latex a) " \\times " (#%latex b))]
    [(_ ((~literal *) a b ...)) #'(string-append (#%latex a) " \\times " (#%latex (* b ...)))]
    [(_ ((~literal -) a b)) #'(string-append (#%latex a) " - " (#%latex b))]
    [(_ ((~literal -) a b ...)) #'(string-append (#%latex a) " - " (#%latex (- b ...)))]
    [(_ ((~literal sqrt) a)) #'(string-append "\\sqrt{" (#%latex a) "}")]
    [(_ ((~literal expt) (a ...) e)) #'(string-append "\\left( " (#%latex (a ...))
                                                      " \\right)^{" (#%latex e) "}")] 
    [(_ ((~literal expt) a e)) #'(string-append (#%latex a) "^{ " (#%latex e) " }")]
    [(_ ((~literal posn #:phase 1) x y)) #'(string-append "(" (#%latex x) "," (#%latex y) ")")]
    [(_ a) #'(convert-to-string a)])



;; [U String Symbol Number] -> String
;; converts a string symbol or a number into their string representation
(define (convert-to-string x)
  (cond
    [(string? x) x]
    [(symbol? x) (symbol->string x)]
    [(number? x) (number->string x)]))