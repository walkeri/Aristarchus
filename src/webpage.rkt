#lang racket

(require xml
         browser/external
         "latex.rkt")


(provide
 generate-webpage
 posn
 posn-x
 posn-y
 )

(struct posn [x y]
  #:transparent)

;; A Latexpression is one of:
;; - String
;; - Symbol
;; - Number
;; - (list 'keep-flat [Listof Latexpression])
;; - (list Symbol [Listof Latexpression] ...)

;; Latexpression-> xexpr
;; generates a latex formula without values substituted in
(define (generate-formula latex)
  `(p ,(string-append "$$" latex "$$")))

;; A Unit is one of:
;; - "kilograms"
;; - "meters"
;; - "seconds"
;; - "meters / seconds^2 



;; [Listof LPair] -> xexpr
;; generates a list of variables with their associated values and units
(define (generate-variables l)
  (append `(ul
            ,@(map generate-one-variable l))))

;; LPair -> xexpr
;; generates a variable with its associated value and unit
(define (generate-one-variable lt)
  `(li "$$" ,(string-append (symbol->string (first lt)) ": ")
       ,(number->string (second lt))"$$"))

;; Latexpression -> xexpr
;; generates a latex formula with values substituted in 
(define (substitute latex)
  `(p ,(string-append "$$" latex "$$")))

;; Posn -> String
;; converts a posn to a string
(define (posn->string p)
  (string-append "(" (number->string (posn-x p)) "," (number->string (posn-y p)) ")"))

;; Number Unit -> xexpr
;; gives back the answer for question
(define (answer n)
  (cond
    [(number? n) `(p ,(string-append "$$" (number->string n) "$$"))]
    [else `(p ,(string-append "$$" (posn->string n) "$$"))]))

(define for-latex
  "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-MML-AM_CHTML")

;; String Latexpression [Listof LPair] Latexpression Number String -> xml
;; generates a webpage for a formula
(define (generate-webpage name l-vars lop l-sub ans)
  (with-output-to-file "../src/output.html"
    #:exists 'replace
    (lambda ()
      (display-xml/content
       (xexpr->xml
        `(html
          (head
           (meta ((charset "utf-8")))
           (meta ((name "viewport")
                  (content "width=device-width")))
           (title "Aristarchus")
           (script ((type "text/javascript")
                    (async "")
                    (src ,for-latex))))
          (body
           (p ,(string-append "The formula for " name " " "is: "))
           ,(generate-formula l-vars)
           (p "Where: ")
           ,(generate-variables lop)
           (p "Substituting yields: ")
           ,(substitute l-sub)
           (p "Simplified: ")
           ,(answer ans)))))))
  (send-url "C:/Users/Isaac/Documents/GitHub/Aristarchus/src/output.html"))

