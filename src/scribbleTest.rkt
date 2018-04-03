#lang racket
(require html
         xml)

(make-html '()
           (list 
 (make-head '()
  (list (make-meta (list (make-attribute (make-location #f #f 0) (make-location #f #f 0) 'charset "utf-8")))
  (make-meta (list (make-attribute (make-location #f #f 0) (make-location #f #f 0)'name "viewport")
                   (make-attribute (make-location #f #f 0) (make-location #f #f 0)'content "width=device-width")))
  (make-title '() (list (make-pcdata (make-location #f #f 0) (make-location #f #f 0) "MathJax example")))
  (make-script (list (make-attribute (make-location #f #f 0) (make-location #f #f 0) 'type "test/javascript")
                     (make-attribute (make-location #f #f 0) (make-location #f #f 0) 'src  "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.3/MathJax.js?config=TeX-MML-AM_CHTML"))
               '())))
  (make-body '()
   (list (make-p '() (list (make-pcdata (make-location #f #f 0) (make-location #f #f 0)
                                            "When \\(a \\ne 0\\), there are two solutions to \\(ax^2 + bx + c = 0\\) and they are
  $$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$")))))))))
