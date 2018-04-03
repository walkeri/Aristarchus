#lang racket

(require xml
         browser/external)

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
     (title "MathJax example")
     (script ((type "text/javascript")
              (async "")
              (src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-MML-AM_CHTML"))))
     (body
      (p
       "When \\(mass = 5.972e+24\\) and \\(radius = 6.3781e+6\\) the surface gravity is 
       $$\\frac{G \\times M_b}{r^2}$$")))))))


(send-url "C:/Users/Isaac/Documents/GitHub/Aristarchus/src/output.html")

#|
    ___         _      __                  __              
   /   |  _____(_)____/ /_____ ___________/ /_  __  _______
  / /| | / ___/ / ___/ __/ __ `/ ___/ ___/ __ \/ / / / ___/
 / ___ |/ /  / (__  ) /_/ /_/ / /  / /__/ / / / /_/ (__  ) 
/_/  |_/_/  /_/____/\__/\__,_/_/   \___/_/ /_/\__,_/____/  
                                                           

|#     