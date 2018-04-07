#lang racket

(require xml
         browser/external
         "formulatex.rkt")


(provide

 ;generate-formula
 ;generate-variables
 ;substitute
 ;answer
 generate-webpage
 posn
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

;; An LTriple is a (list Id Number Unit)

;; [Listof LTriple] -> xexpr
;; generates a list of variables with their associated values and units
(define (generate-variables l)
  (append `(ul
            ,@(map generate-one-variable l))))

;; LTriple -> xexpr
;; generates a variable with its associated value and unit
(define (generate-one-variable lt)
  `(li "$$" ,(string-append (symbol->string (first lt)) ": ")
       ,(number->string (second lt))
       ,(third lt) "$$"))

;; Latexpression -> xexpr
;; generates a latex formula with values substituted in 
(define (substitute latex)
  `(p ,(string-append "$$" latex "$$")))

;; Posn -> String
;; converts a posn to a string
(define (posn->string p u)
  (string-append "(" (number->string (posn-x p)) u "," (number->string (posn-y p)) u ")"))

;; Number Unit -> xexpr
;; gives back the answer for question
(define (answer n u)
  (cond
    [(number? n) `(p ,(string-append "$$" (number->string n) " " u "$$"))]
    [(posn? n) `(p ,(string-append "$$" (posn->string n u) "$$"))]))

(define for-latex
  "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-MML-AM_CHTML")

;; String Latexpression [Listof LTriple] Latexpression Number String -> xml
;; generates a webpage for a formula
(define (generate-webpage name l-vars LoLT l-sub ans ans-unit)
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
           ,(generate-variables LoLT)
           (p "Substituting yields: ")
           ,(substitute l-sub)
           (p "Simplified: ")
           ,(answer ans ans-unit)))))))
  (send-url "C:/Users/Isaac/Documents/GitHub/Aristarchus/src/output.html"))


