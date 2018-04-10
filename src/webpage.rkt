#lang racket

(require xml
         browser/external
         "built-in-structures.rkt")


(provide

 ;; String Symbol String [Listof LPair] String Number -> VOID
 ;; 
 generate-webpage
 )

;; [Listof LPair] -> xexpr
;; generates a list of variables with their associated values and units
(define (assignment-table l)
  `(table
    ,@(map assignment-row l)))

;; LPair -> xexpr
;; Generates a table row showing the assignment of the formula variables to values
(define (assignment-row lt)
  `(tr (td ,(wrap-$$ (symbol->string (first lt))))
       (td "=")
       (td ,(wrap-$$ (number->string (second lt))))))

;; String -> String
(define (wrap-$$ s)
  (string-append "$$" s "$$"))

;; Location of JS library used for LaTeX rendering.
(define MATH-JAX
  "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-MML-AM_CHTML")

;; Latexpression [Listof LPair] Latexpression Number String -> xml
;; generates a webpage for a formula

(define (generate-webpage formula-name with-variables lop with-values answer)
  (begin
    (define file-name
      (string-append "C:/Users/Isaac/Documents/GitHub/Aristarchus/src/html/"
                     (symbol->string (gensym))
                     ".html"))
    (with-output-to-file file-name
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
                        (src ,MATH-JAX))))
              (body
               (p ,(string-append "The formula for " (symbol->string formula-name) " is: "))
               (p ,(wrap-$$ with-variables))
               (p "Where: ")
               (p ,(assignment-table lop))
               (p "Substitution yields: ")
               (p ,(wrap-$$ with-values))
               (p "Simplified: ")
               (p ,(wrap-$$ (answer->string answer)))))))))
      (send-url file-name)))

;; Any -> String
;; converts a number or a posn to a string, and errors if it is neither of those things
(define (answer->string a)
  (cond
    [(number? a) (number->string a)]
    [(posn? a) (posn->string a)]
    [else (error "Answer not a posn or a number.")]))

;; Posn -> String
;; converts a posn into a coordinate pair string.
(define (posn->string a)
  (string-append "(" (number->string (posn-x a)) "," (number->string (posn-y a)) ")"))

