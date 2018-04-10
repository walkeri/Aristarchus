#lang racket

(require (for-syntax syntax/parse))

(provide
 
 ;; (#%latex MSE) transforms the MSE into syntax that evaluates to the string representing it in LaTeX
 #%latex)

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

(define-syntax (#%latex stx)
  (syntax-parse stx
    [(_ ((~literal /) a b)) #'(string-append "\\frac{" (#%latex a) "}{" (#%latex b) "}")]
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
    [(_ ((~literal posn) x y)) #'(string-append "posn(" (#%latex x) "," (#%latex y) ")")]
    [(_ a) #'(convert-to-string a)]))

(define (convert-to-string x)
  (cond
    [(string? x) x]
    [(symbol? x) (symbol->string x)]
    [(number? x) (number->string x)]))




